(* Sobol quasi-random sequence (NR 7.8). *)

let fmt6 v =
  let w = if abs_float v < 5e-7 then 0.0 else v in
  Printf.sprintf "%.6f" (w +. 0.0)

let canon v = if abs_float v < 5e-7 then 0.0 else v

let () =
  let toks =
    String.split_on_char ' '
      (String.concat " "
         (let rec rd acc =
            try rd (input_line stdin :: acc) with End_of_file -> List.rev acc
          in
          rd []))
  in
  let toks = Array.of_list (List.filter (fun s -> s <> "") toks) in
  if Array.length toks = 0 then exit 0;
  if toks.(0) <> "SOBOL" then exit 0;
  let dim = int_of_string toks.(1) and npoints = int_of_string toks.(2) in
  let maxbit = 30 and maxdim = 6 in
  let mdeg = [| 1; 2; 3; 3; 4; 4 |] and ip = [| 0; 1; 1; 2; 1; 4 |] in
  let ivinit =
    [| 1; 1; 1; 1; 1; 1; 3; 1; 3; 3; 1; 1; 5; 7; 7; 3; 3; 5; 15; 11; 5; 15; 13; 9 |]
  in
  let m32 = 0xFFFFFFFF in
  let ix = Array.make maxdim 0 in
  let iu = Array.make_matrix maxbit maxdim 0 in
  for j = 0 to 3 do
    for k = 0 to maxdim - 1 do
      iu.(j).(k) <- ivinit.(j * maxdim + k)
    done
  done;
  let fac = 1.0 /. float (1 lsl maxbit) in
  for k = 0 to maxdim - 1 do
    for j = 0 to mdeg.(k) - 1 do
      iu.(j).(k) <- (iu.(j).(k) lsl (maxbit - 1 - j)) land m32
    done;
    for j = mdeg.(k) to maxbit - 1 do
      let ipp = ref ip.(k) and i = ref iu.(j - mdeg.(k)).(k) in
      i := !i lxor (!i lsr mdeg.(k));
      for l = mdeg.(k) - 1 downto 1 do
        (if !ipp land 1 <> 0 then i := !i lxor iu.(j - l).(k));
        ipp := !ipp lsr 1
      done;
      iu.(j).(k) <- !i land m32
    done
  done;
  let inn = ref 0 in
  for _pt = 1 to npoints do
    let im = ref !inn in
    inn := !inn + 1;
    let j = ref 0 in
    while !j < maxbit && !im land 1 <> 0 do
      im := !im lsr 1;
      j := !j + 1
    done;
    let words = ref [] in
    for k = 0 to dim - 1 do
      ix.(k) <- ix.(k) lxor iu.(!j).(k);
      words := fmt6 (canon (float ix.(k) *. fac)) :: !words
    done;
    print_endline (String.concat " " (List.rev !words))
  done
