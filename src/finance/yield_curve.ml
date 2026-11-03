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
    let n = int_of_string tokens.(0) in
    let y = Array.init n (fun i -> float_of_string tokens.(1 + i)) in
    let df = Array.make n 0.0 in
    for j = 0 to n - 1 do
      let cp = y.(j) *. 50.0 in
      let acc = ref 0.0 in
      for k = 0 to j - 1 do
        acc := !acc +. cp *. df.(k)
      done;
      df.(j) <- (100.0 -. !acc) /. (100.0 +. cp)
    done;
    print_string "ZEROS:";
    for j = 0 to n - 1 do
      let z = 2.0 *. ((df.(j) ** (-1.0 /. float_of_int (j + 1))) -. 1.0) in
      Printf.printf " %.6f" z
    done;
    print_string "\nFORWARDS:";
    for j = 1 to n - 1 do
      let f = 2.0 *. (df.(j - 1) /. df.(j) -. 1.0) in
      Printf.printf " %.6f" f
    done;
    print_newline ()
  end
