(* Dawson's integral (NR 6.9). *)

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
  let x = float_of_string toks.(0) in
  let nmax = 6 in
  let hstep = 0.4 in
  let a1 = 2.0 /. 3.0 and a2 = 0.4 and a3 = 2.0 /. 7.0 in
  let c = Array.init nmax (fun i ->
      let t = (2.0 *. float i +. 1.0) *. hstep in
      exp (-.t *. t))
  in
  let ans =
    if abs_float x < 0.2 then (
      let x2 = x *. x in
      x *. (1.0 -. a1 *. x2 *. (1.0 -. a2 *. x2 *. (1.0 -. a3 *. x2))))
    else (
      let xx = abs_float x in
      let n0 = 2 * int_of_float (0.5 *. xx /. hstep +. 0.5) in
      let xp = xx -. float n0 *. hstep in
      let e1 = ref (exp (2.0 *. xp *. hstep)) in
      let e2 = !e1 *. !e1 in
      let d1 = ref (float n0 +. 1.0) and d2 = ref (float n0 +. 1.0 -. 2.0) in
      let sum = ref 0.0 in
      for _i = 0 to nmax - 1 do
        sum := !sum +. c.(_i) *. (!e1 /. !d1 +. 1.0 /. (!d2 *. !e1));
        d1 := !d1 +. 2.0;
        d2 := !d2 -. 2.0;
        e1 := !e1 *. e2
      done;
      let s = if x >= 0.0 then exp (-.xp *. xp) else -.exp (-.xp *. xp) in
      0.5641895835 *. s *. !sum)
  in
  print_endline (fmt6 (canon ans))
