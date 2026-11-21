(* Adaptive quadrature (NR 4.7). Port of the C++ reference. *)

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
  let fun_ = next_tok () in
  let p =
    if fun_ = "POLY" then (
      let d = int_of_string (next_tok ()) in
      Array.init (d + 1) (fun _ -> float_of_string (next_tok ())))
    else if fun_ = "POW" then Array.init 4 (fun _ -> float_of_string (next_tok ()))
    else Array.init 3 (fun _ -> float_of_string (next_tok ()))
  in
  let a = float_of_string (next_tok ()) in
  let b = float_of_string (next_tok ()) in
  let eps = float_of_string (next_tok ()) in
  let f t =
    if fun_ = "POLY" then (
      let s = ref 0.0 in
      for i = Array.length p - 1 downto 0 do
        s := !s *. t +. p.(i)
      done;
      !s)
    else if fun_ = "EXP" then p.(0) *. exp (p.(1) *. t) +. p.(2)
    else if fun_ = "TRIG" then p.(0) *. sin (p.(1) *. t +. p.(2))
    else if fun_ = "LOG" then p.(0) *. log (p.(1) *. t +. p.(2))
    else if fun_ = "GAUSS" then (
      let z = (t -. p.(1)) /. p.(2) in
      p.(0) *. exp (-.z *. z))
    else p.(0) *. ((p.(1) *. t +. p.(2)) ** p.(3))
  in
  let r5 = 1.0 /. sqrt 5.0 and r23 = sqrt (2.0 /. 3.0) in
  let lobatto4 aa bb =
    let m = 0.5 *. (aa +. bb) and h = 0.5 *. (bb -. aa) in
    h
    *. ((f (m -. h) +. f (m +. h)) /. 6.0
       +. 5.0 *. (f (m -. h *. r5) +. f (m +. h *. r5)) /. 6.0)
  in
  let kronrod7 aa bb =
    let m = 0.5 *. (aa +. bb) and h = 0.5 *. (bb -. aa) in
    h
    *. (11.0 *. (f (m -. h) +. f (m +. h)) /. 210.0
       +. 72.0 *. (f (m -. h *. r23) +. f (m +. h *. r23)) /. 245.0
       +. 125.0 *. (f (m -. h *. r5) +. f (m +. h *. r5)) /. 294.0
       +. 16.0 *. f m /. 35.0)
  in
  let iscale = kronrod7 a b in
  let oot = ref false in
  let rec rec_ aa bb depth =
    let i1 = kronrod7 aa bb and i2 = lobatto4 aa bb in
    let m = 0.5 *. (aa +. bb) in
    if abs_float (i1 -. i2) <= eps *. abs_float iscale then i1
    else if depth >= 12 || m <= aa || bb <= m then (
      oot := true;
      i1)
    else rec_ aa m (depth + 1) +. rec_ m bb (depth + 1)
  in
  let ans = rec_ a b 0 in
  print_string (fmt6 (canon ans));
  print_string (if !oot then " 1\n" else " 0\n")
