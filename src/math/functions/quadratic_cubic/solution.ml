(* Quadratic and cubic equations (NR 5.6). *)

let fmt6 v =
  let w = if abs_float v < 5e-7 then 0.0 else v in
  Printf.sprintf "%.6f" (w +. 0.0)

let canon v = if abs_float v < 5e-7 then 0.0 else v
let fmt2 r im = fmt6 (canon r) ^ " " ^ fmt6 (canon im)
let pi = 3.141592653589793

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
  let next () =
    let v = toks.(!pos) in
    incr pos; v
  in
  let op = next () in
  if op = "QUAD" then (
    let a = float_of_string (next ()) in
    let b = float_of_string (next ()) in
    let c = float_of_string (next ()) in
    let disc = b *. b -. 4.0 *. a *. c in
    if disc >= 0.0 then (
      let sq = sqrt disc in
      let q = -0.5 *. (b +. (if b < 0.0 then -.sq else sq)) in
      let x1, x2 = if q = 0.0 then (0.0, 0.0) else (q /. a, c /. q) in
      print_endline (fmt2 x1 0.0);
      print_endline (fmt2 x2 0.0))
    else (
      let sr = ref 0.0 and si = ref (sqrt (-.disc)) in
      if b *. !sr < 0.0 then (
        sr := -. !sr;
        si := -. !si);
      let qr = -0.5 *. (b +. !sr) and qi = -0.5 *. !si in
      if qr = 0.0 && qi = 0.0 then (
        print_endline (fmt2 0.0 0.0);
        print_endline (fmt2 0.0 0.0))
      else (
        let den = qr *. qr +. qi *. qi in
        print_endline (fmt2 (qr /. a) (qi /. a));
        print_endline (fmt2 (c *. qr /. den) (-.c *. qi /. den)))))
  else if op = "CUBIC" then (
    let a = float_of_string (next ()) in
    let b = float_of_string (next ()) in
    let c = float_of_string (next ()) in
    let q = (a *. a -. 3.0 *. b) /. 9.0 in
    let r = (2.0 *. a *. a *. a -. 9.0 *. a *. b +. 27.0 *. c) /. 54.0 in
    if r *. r < q *. q *. q then (
      let th = acos (max (-1.0) (min 1.0 (r /. sqrt (q *. q *. q)))) in
      let s = -2.0 *. sqrt q in
      print_endline (fmt2 (s *. cos (th /. 3.0) -. a /. 3.0) 0.0);
      print_endline (fmt2 (s *. cos ((th +. 2.0 *. pi) /. 3.0) -. a /. 3.0) 0.0);
      print_endline (fmt2 (s *. cos ((th -. 2.0 *. pi) /. 3.0) -. a /. 3.0) 0.0))
    else (
      let sq = sqrt (max 0.0 (r *. r -. q *. q *. q)) in
      let av = -. (if r < 0.0 then -1.0 else 1.0) *. ((abs_float r +. sq) ** (1.0 /. 3.0)) in
      let bv = if av = 0.0 then 0.0 else q /. av in
      let x1 = av +. bv -. a /. 3.0 in
      let re = -0.5 *. (av +. bv) -. a /. 3.0 in
      let im = sqrt 3.0 /. 2.0 *. (av -. bv) in
      print_endline (fmt2 x1 0.0);
      print_endline (fmt2 re im);
      print_endline (fmt2 re (-.im))))
  else if op = "ASINH" then (
    let x = float_of_string (next ()) in
    let v =
      if x >= 0.0 then log (x +. sqrt (x *. x +. 1.0))
      else -. log (-.x +. sqrt (x *. x +. 1.0))
    in
    print_endline (fmt6 (canon v)))
  else (
    let x = float_of_string (next ()) in
    let v = log (x +. sqrt (x *. x -. 1.0)) in
    print_endline (fmt6 (canon v)))
