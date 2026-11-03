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
  if Array.length tokens >= 1 then begin
    let pos = ref 0 in
    let next_str () = let t = tokens.(!pos) in incr pos; t in
    let next_int () = int_of_string (next_str ()) in
    let next_int64 () = Int64.of_string (next_str ()) in
    let q = next_int () in
    let is_ext = Array.make q false in
    let val_arr = Array.make q 0L in
    for i = 0 to q - 1 do
      let op = next_int () in
      if op = 1 then val_arr.(i) <- next_int64 ()
      else is_ext.(i) <- true
    done;
    let e_list = ref [] in
    for i = 0 to q - 1 do
      if is_ext.(i) then e_list := i :: !e_list
    done;
    let e_arr = Array.of_list (List.rev !e_list) in
    let m = Array.length e_arr in
    let ins_list = ref [] in
    for i = 0 to q - 1 do
      if not is_ext.(i) then ins_list := (val_arr.(i), i) :: !ins_list
    done;
    let ins_arr = Array.of_list (List.rev !ins_list) in
    Array.sort (fun (v1, _) (v2, _) -> Int64.compare v1 v2) ins_arr;
    let p = Array.init (m + 1) (fun i -> i) in
    let rec find x =
      if p.(x) = x then x
      else begin
        p.(x) <- find p.(x);
        p.(x)
      end
    in
    let occupy x = p.(x) <- find (x + 1) in
    let ans = Array.make m 0L in
    let has = Array.make m false in
    let upper_bound target =
      let low = ref 0 in
      let high = ref m in
      while !low < !high do
        let mid = (!low + !high) / 2 in
        if e_arr.(mid) <= target then low := mid + 1
        else high := mid
      done;
      !low
    in
    for k = 0 to Array.length ins_arr - 1 do
      let (v, t) = ins_arr.(k) in
      let j0 = upper_bound t in
      let j = find j0 in
      if j < m then begin
        ans.(j) <- v;
        has.(j) <- true;
        occupy j
      end
    done;
    for j = 0 to m - 1 do
      if has.(j) then Printf.printf "%Ld\n" ans.(j)
      else print_endline "EMPTY"
    done
  end
