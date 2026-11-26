(* Multidimensional grid quadrature (NR 4.8). *)

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
  let pos = ref 0 in
  let next_tok () =
    let v = toks.(!pos) in
    incr pos; v
  in
  let f3op = next_tok () in
  let np = if f3op = "P3" then 10 else if f3op = "E3" then 4 else 5 in
  let p = Array.init np (fun _ -> float_of_string (next_tok ())) in
  let x1 = float_of_string (next_tok ()) in
  let x2 = float_of_string (next_tok ()) in
  let read_bound nparams =
    let op = next_tok () in
    if op = "C" then (op, [| float_of_string (next_tok ()) |])
    else (op, Array.init nparams (fun _ -> float_of_string (next_tok ())))
  in
  let y1op, y1p = read_bound 2 in
  let y2op, y2p = read_bound 2 in
  let z1op, z1p = read_bound 3 in
  let z2op, z2p = read_bound 3 in
  let f x y z =
    if f3op = "P3" then
      p.(0) +. p.(1) *. x +. p.(2) *. y +. p.(3) *. z +. p.(4) *. x *. x
      +. p.(5) *. y *. y
      +. p.(6) *. z *. z
      +. p.(7) *. x *. y
      +. p.(8) *. x *. z
      +. p.(9) *. y *. z
    else if f3op = "E3" then p.(0) *. exp (p.(1) *. x +. p.(2) *. y +. p.(3) *. z)
    else p.(0) *. sin (p.(1) *. x +. p.(2) *. y +. p.(3) *. z +. p.(4))
  in
  let ylo x = if y1op = "C" then y1p.(0) else y1p.(0) *. x +. y1p.(1) in
  let yhi x = if y2op = "C" then y2p.(0) else y2p.(0) *. x +. y2p.(1) in
  let zlo x y = if z1op = "C" then z1p.(0) else z1p.(0) +. z1p.(1) *. x +. z1p.(2) *. y in
  let zhi x y = if z2op = "C" then z2p.(0) else z2p.(0) +. z2p.(1) *. x +. z2p.(2) *. y in
  let nq = 10 and eps = 1.0e-14 in
  let gx = Array.make nq 0.0 and gw = Array.make nq 0.0 in
  let m = (nq + 1) / 2 in
  for i = 0 to m - 1 do
    let z = ref (cos (3.141592654 *. (float i +. 0.75) /. (float nq +. 0.5))) in
    let pp = ref 0.0 in
    (try
       while true do
         let p1 = ref 1.0 and p2 = ref 0.0 in
         for j = 0 to nq - 1 do
           let p3 = !p2 in
           p2 := !p1;
           p1 := ((2.0 *. float j +. 1.0) *. !z *. !p2 -. float j *. p3) /. float (j + 1)
         done;
         pp := float nq *. (!z *. !p1 -. !p2) /. (!z *. !z -. 1.0);
         let z1 = !z in
         z := z1 -. !p1 /. !pp;
         if abs_float (!z -. z1) <= eps then raise Exit
       done
     with Exit -> ());
    gx.(i) <- -. !z;
    gx.(nq - 1 - i) <- !z;
    gw.(i) <- 2.0 /. ((1.0 -. !z *. !z) *. !pp *. !pp);
    gw.(nq - 1 - i) <- gw.(i)
  done;
  let ans = ref 0.0 in
  for i = 0 to nq - 1 do
    let x = 0.5 *. (x1 +. x2) +. 0.5 *. (x2 -. x1) *. gx.(i) in
    let yl = ylo x and yh = yhi x in
    for j = 0 to nq - 1 do
      let y = 0.5 *. (yl +. yh) +. 0.5 *. (yh -. yl) *. gx.(j) in
      let zl = zlo x y and zh = zhi x y in
      let inner = ref 0.0 in
      for k = 0 to nq - 1 do
        let z = 0.5 *. (zl +. zh) +. 0.5 *. (zh -. zl) *. gx.(k) in
        inner := !inner +. gw.(k) *. f x y z
      done;
      ans := !ans +. gw.(i) *. gw.(j) *. 0.125 *. (x2 -. x1) *. (yh -. yl) *. (zh -. zl) *. !inner
    done
  done;
  print_endline (fmt6 (canon !ans))
