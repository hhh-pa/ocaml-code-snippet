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
  if Array.length tokens >= 3 then begin
    let d = float_of_string tokens.(0) in
    let l = int_of_string tokens.(1) in
    let m = int_of_string tokens.(2) in
    let x = Array.init m (fun i -> float_of_string tokens.(3 + i)) in
    let w = Array.make l 0.0 in
    w.(0) <- 1.0;
    for k = 1 to l - 1 do
      w.(k) <- -. w.(k - 1) *. (d -. float_of_int k +. 1.0) /. float_of_int k
    done;
    Printf.printf "W:";
    Array.iter (fun v -> Printf.printf " %.6f" (v +. 0.0)) w;
    Printf.printf "\nY:";
    for t = l - 1 to m - 1 do
      let s = ref 0.0 in
      for k = 0 to l - 1 do
        s := !s +. w.(k) *. x.(t - k)
      done;
      Printf.printf " %.6f" (!s +. 0.0)
    done;
    Printf.printf "\n"
  end
