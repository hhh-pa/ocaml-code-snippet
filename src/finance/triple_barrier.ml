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
  if Array.length tokens >= 4 then begin
    let pos = ref 0 in
    let next_str () = let t = tokens.(!pos) in incr pos; t in
    let next_int () = int_of_string (next_str ()) in
    let next_float () = float_of_string (next_str ()) in
    let m = next_int () in
    let h = next_int () in
    let up = next_float () in
    let dn = next_float () in
    let px = Array.init m (fun _ -> next_float ()) in
    let e = next_int () in
    let ev = Array.make e 0 in
    let side = Array.make e 0 in
    for j = 0 to e - 1 do
      ev.(j) <- next_int ();
      side.(j) <- next_int ()
    done;
    let lab = Array.make e 0 in
    let meta = Array.make e 0 in
    for j = 0 to e - 1 do
      let t0 = ev.(j) in
      let l = ref 0 in
      let t = ref (t0 + 1) in
      let max_t = min (m - 1) (t0 + h) in
      let stop = ref false in
      while !t <= max_t && not !stop do
        let r = px.(!t) -. px.(t0) in
        if r >= up -. 1e-12 then begin
          l := 1;
          stop := true
        end else if r <= -.dn +. 1e-12 then begin
          l := -1;
          stop := true
        end;
        incr t
      done;
      lab.(j) <- !l;
      meta.(j) <- (if !l <> 0 && side.(j) = !l then 1 else 0)
    done;
    print_string "LABELS:";
    for j = 0 to e - 1 do
      Printf.printf " %d" lab.(j)
    done;
    print_string "\nMETA:";
    for j = 0 to e - 1 do
      Printf.printf " %d" meta.(j)
    done;
    print_newline ()
  end
