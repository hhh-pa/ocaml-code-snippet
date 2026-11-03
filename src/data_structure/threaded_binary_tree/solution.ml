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

    let cap = ref (max 32 (n + 2)) in
    let key = ref (Array.make !cap 0L) in
    let l_arr = ref (Array.make !cap (-1)) in
    let r_arr = ref (Array.make !cap (-1)) in
    let lt = ref (Array.make !cap true) in
    let rt = ref (Array.make !cap true) in
    let count = ref 0 in
    let root = ref (-1) in

    let alloc x =
      if !count >= !cap then begin
        let new_cap = !cap * 2 in
        let grow_i64 a =
          let na = Array.make new_cap 0L in
          Array.blit !a 0 na 0 !cap;
          a := na
        in
        let grow_int a =
          let na = Array.make new_cap (-1) in
          Array.blit !a 0 na 0 !cap;
          a := na
        in
        let grow_bool a =
          let na = Array.make new_cap true in
          Array.blit !a 0 na 0 !cap;
          a := na
        in
        grow_i64 key;
        grow_int l_arr;
        grow_int r_arr;
        grow_bool lt;
        grow_bool rt;
        cap := new_cap
      end;
      let idx = !count in
      incr count;
      (!key).(idx) <- x;
      (!l_arr).(idx) <- -1;
      (!r_arr).(idx) <- -1;
      (!lt).(idx) <- true;
      (!rt).(idx) <- true;
      idx
    in

    let insert x =
      let node = alloc x in
      if !root = -1 then root := node
      else begin
        let cur = ref !root in
        let stop = ref false in
        while not !stop do
          if x = (!key).(!cur) then begin
            decr count;
            stop := true
          end else if x < (!key).(!cur) then begin
            if (!lt).(!cur) then begin
              (!l_arr).(node) <- (!l_arr).(!cur);
              (!r_arr).(node) <- !cur;
              (!l_arr).(!cur) <- node;
              (!lt).(!cur) <- false;
              let ln = (!l_arr).(node) in
              if ln <> -1 && (!rt).(ln) then (!r_arr).(ln) <- node;
              stop := true
            end else cur := (!l_arr).(!cur)
          end else begin
            if (!rt).(!cur) then begin
              (!r_arr).(node) <- (!r_arr).(!cur);
              (!l_arr).(node) <- !cur;
              (!r_arr).(!cur) <- node;
              (!rt).(!cur) <- false;
              let rn = (!r_arr).(node) in
              if rn <> -1 && (!lt).(rn) then (!l_arr).(rn) <- node;
              stop := true
            end else cur := (!r_arr).(!cur)
          end
        done
      end
    in

    for _ = 0 to n - 1 do
      insert (next_int64 ())
    done;

    let pred_node i =
      if (!lt).(i) then (!l_arr).(i)
      else begin
        let j = ref (!l_arr).(i) in
        while not (!rt).(!j) do
          j := (!r_arr).(!j)
        done;
        !j
      end
    in

    let succ_node i =
      if (!rt).(i) then (!r_arr).(i)
      else begin
        let j = ref (!r_arr).(i) in
        while not (!lt).(!j) do
          j := (!l_arr).(!j)
        done;
        !j
      end
    in

    let find x =
      let lo = ref (-1) in
      let hi = ref (-1) in
      let cur = ref !root in
      let found = ref (-1) in
      let stop = ref false in
      while !cur <> -1 && not !stop do
        if x = (!key).(!cur) then begin
          found := !cur;
          stop := true
        end else if x < (!key).(!cur) then begin
          hi := !cur;
          if (!lt).(!cur) then stop := true
          else cur := (!l_arr).(!cur)
        end else begin
          lo := !cur;
          if (!rt).(!cur) then stop := true
          else cur := (!r_arr).(!cur)
        end
      done;
      (!found, !lo, !hi)
    in

    let pred x =
      let (f, lo, _) = find x in
      let p = if f = -1 then lo else pred_node f in
      if p = -1 then None else Some (!key).(p)
    in

    let succ x =
      let (f, _, hi) = find x in
      let s = if f = -1 then hi else succ_node f in
      if s = -1 then None else Some (!key).(s)
    in

    let inorder () =
      if !root = -1 then print_newline ()
      else begin
        let cur = ref !root in
        while not (!lt).(!cur) do
          cur := (!l_arr).(!cur)
        done;
        let first = ref true in
        while !cur <> -1 do
          if not !first then print_string " ";
          first := false;
          Printf.printf "%Ld" (!key).(!cur);
          cur := succ_node !cur
        done;
        print_newline ()
      end
    in

    for _ = 0 to q - 1 do
      if !pos < Array.length tokens then begin
        let op = next_int () in
        if op = 1 then begin
          let x = next_int64 () in
          match pred x with
          | Some v -> Printf.printf "%Ld\n" v
          | None -> print_endline "NONE"
        end else if op = 2 then begin
          let x = next_int64 () in
          match succ x with
          | Some v -> Printf.printf "%Ld\n" v
          | None -> print_endline "NONE"
        end else if op = 3 then begin
          inorder ()
        end
      end
    done
  end
