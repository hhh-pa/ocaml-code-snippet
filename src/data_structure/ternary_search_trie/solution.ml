type tst_node = {
  ch : char;
  mutable has_val : bool;
  mutable val_data : int64;
  mutable left : int;
  mutable mid : int;
  mutable right : int;
}

let () =
  let tokens =
    let buf = Buffer.create 4096 in
    (try while true do Buffer.add_channel buf stdin 4096 done with End_of_file -> ());
    let s = Buffer.contents buf in
    let len = String.length s in
    let res = ref [] in
    let i = ref 0 in
    while !i < len do
      while !i < len && (let c = s.[!i] in c = ' ' || c = '\t' || c = '\r' || c = '\n') do incr i done;
      if !i < len then begin
        let st = !i in
        while !i < len && (let c = s.[!i] in c <> ' ' && c <> '\t' && c <> '\r' && c <> '\n') do incr i done;
        res := String.sub s st (!i - st) :: !res
      end
    done;
    Array.of_list (List.rev !res)
  in
  if Array.length tokens >= 2 then begin
    let pos = ref 0 in
    let next_str () = let t = tokens.(!pos) in incr pos; t in
    let next_int () = int_of_string (next_str ()) in
    let next_int64 () = Int64.of_string (next_str ()) in
    let n = next_int () in
    let q = next_int () in

    let cap = ref 256 in
    let nodes = ref (Array.init !cap (fun _ -> { ch = ' '; has_val = false; val_data = 0L; left = -1; mid = -1; right = -1 })) in
    let count = ref 0 in
    let root = ref (-1) in

    let alloc_node c =
      if !count >= !cap then begin
        let new_cap = !cap * 2 in
        let new_nodes = Array.init new_cap (fun i ->
          if i < !cap then (!nodes).(i)
          else { ch = ' '; has_val = false; val_data = 0L; left = -1; mid = -1; right = -1 }
        ) in
        nodes := new_nodes;
        cap := new_cap
      end;
      let idx = !count in
      incr count;
      (!nodes).(idx) <- { ch = c; has_val = false; val_data = 0L; left = -1; mid = -1; right = -1 };
      idx
    in

    let rec put u key v d =
      let c = key.[d] in
      let u_idx = if u = -1 then alloc_node c else u in
      let node = (!nodes).(u_idx) in
      if c < node.ch then begin
        let l = put node.left key v d in
        (!nodes).(u_idx).left <- l
      end else if c > node.ch then begin
        let r = put node.right key v d in
        (!nodes).(u_idx).right <- r
      end else if d + 1 < String.length key then begin
        let m = put node.mid key v (d + 1) in
        (!nodes).(u_idx).mid <- m
      end else begin
        (!nodes).(u_idx).has_val <- true;
        (!nodes).(u_idx).val_data <- v
      end;
      u_idx
    in

    let insert key v =
      if String.length key > 0 then
        root := put !root key v 0
    in

    for _ = 0 to n - 1 do
      let word = next_str () in
      let v = next_int64 () in
      insert word v
    done;

    let get_node u key =
      let cur = ref u in
      let d = ref 0 in
      let klen = String.length key in
      let result = ref (-1) in
      let stop = ref false in
      while !cur <> -1 && not !stop do
        let c = key.[!d] in
        let node = (!nodes).(!cur) in
        if c < node.ch then cur := node.left
        else if c > node.ch then cur := node.right
        else if !d + 1 < klen then begin
          cur := node.mid;
          incr d
        end else begin
          result := !cur;
          stop := true
        end
      done;
      !result
    in

    let search key =
      if String.length key = 0 then None
      else
        let u = get_node !root key in
        if u <> -1 && (!nodes).(u).has_val then Some (!nodes).(u).val_data
        else None
    in

    let rec collect u prefix_buf res =
      if u <> -1 then begin
        let node = (!nodes).(u) in
        collect node.left prefix_buf res;
        Buffer.add_char prefix_buf node.ch;
        if node.has_val then res := Buffer.contents prefix_buf :: !res;
        collect node.mid prefix_buf res;
        Buffer.truncate prefix_buf (Buffer.length prefix_buf - 1);
        collect node.right prefix_buf res
      end
    in

    let keys_with_prefix prefix =
      if String.length prefix = 0 then []
      else
        let u = get_node !root prefix in
        if u = -1 then []
        else begin
          let res = ref [] in
          if (!nodes).(u).has_val then res := prefix :: !res;
          let buf = Buffer.create 32 in
          Buffer.add_string buf prefix;
          collect (!nodes).(u).mid buf res;
          List.rev !res
        end
    in

    let longest_prefix_of query =
      let qlen = String.length query in
      if qlen = 0 then ""
      else begin
        let u = ref !root in
        let d = ref 0 in
        let best_len = ref 0 in
        let found = ref false in
        while !u <> -1 && !d < qlen do
          let c = query.[!d] in
          let node = (!nodes).(!u) in
          if c < node.ch then u := node.left
          else if c > node.ch then u := node.right
          else begin
            incr d;
            if node.has_val then begin
              best_len := !d;
              found := true
            end;
            u := node.mid
          end
        done;
        if !found then String.sub query 0 !best_len else ""
      end
    in

    let rec collect_match u prefix_buf d pat res =
      if u <> -1 then begin
        let node = (!nodes).(u) in
        let c = pat.[d] in
        let plen = String.length pat in
        if c = '.' || c < node.ch then collect_match node.left prefix_buf d pat res;
        if c = '.' || c = node.ch then begin
          Buffer.add_char prefix_buf node.ch;
          if d + 1 = plen then begin
            if node.has_val then res := Buffer.contents prefix_buf :: !res
          end else
            collect_match node.mid prefix_buf (d + 1) pat res;
          Buffer.truncate prefix_buf (Buffer.length prefix_buf - 1)
        end;
        if c = '.' || c > node.ch then collect_match node.right prefix_buf d pat res
      end
    in

    let keys_that_match pat =
      if String.length pat = 0 then []
      else begin
        let res = ref [] in
        let buf = Buffer.create 32 in
        collect_match !root buf 0 pat res;
        List.rev !res
      end
    in

    for _ = 0 to q - 1 do
      if !pos < Array.length tokens then begin
        let op = next_int () in
        let arg = next_str () in
        if op = 1 then begin
          match search arg with
          | Some v -> Printf.printf "%Ld\n" v
          | None -> print_endline "NOT_FOUND"
        end else if op = 2 then begin
          let res = keys_with_prefix arg in
          if res = [] then print_endline "NONE"
          else print_endline (String.concat " " res)
        end else if op = 3 then begin
          let res = longest_prefix_of arg in
          if res = "" then print_endline "NONE"
          else print_endline res
        end else if op = 4 then begin
          let res = keys_that_match arg in
          if res = [] then print_endline "NONE"
          else print_endline (String.concat " " res)
        end
      end
    done
  end
