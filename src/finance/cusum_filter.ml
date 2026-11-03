let () =
  let tokens =
    let buf = Buffer.create 4096 in
    (try while true do Buffer.add_channel buf stdin 4096 done with End_of_file -> ());
    let s = Buffer.contents buf in
    let len = String.length s in
    let res = ref [] in
    let i = ref 0 in
    while !i < len do
      while !i < len && (s.[!i] = ' ' || s.[!i] = '\t' || s.[!i] = '\r' || s.[!i] = '\n') do incr i done;
      if !i < len then begin
        let st = !i in
        while !i < len && not (s.[!i] = ' ' || s.[!i] = '\t' || s.[!i] = '\r' || s.[!i] = '\n') do incr i done;
        res := String.sub s st (!i - st) :: !res
      end
    done;
    Array.of_list (List.rev !res)
  in
  if Array.length tokens >= 2 then begin
    let h = float_of_string tokens.(0) in
    let m = int_of_string tokens.(1) in
    let px = Array.init m (fun i -> float_of_string tokens.(2 + i)) in
    let mu = (Array.fold_left (+.) 0.0 px) /. float_of_int m in
    let sp = ref 0.0 in
    let sn = ref 0.0 in
    let ev = ref [] in
    for i = 0 to m - 1 do
      let y = px.(i) -. mu in
      sp := !sp +. y; if !sp < 0.0 then sp := 0.0;
      sn := !sn +. y; if !sn > 0.0 then sn := 0.0;
      if !sp > h || !sn < -.h then begin
        ev := i :: !ev;
        sp := 0.0;
        sn := 0.0
      end
    done;
    let ev_list = List.rev !ev in
    Printf.printf "EVENTS:";
    List.iter (fun e -> Printf.printf " %d" e) ev_list;
    Printf.printf "\nCOUNT: %d\n" (List.length ev_list)
  end
