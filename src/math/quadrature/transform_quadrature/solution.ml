(* Quadrature by variable transformation (NR 4.5). Port of the C++ reference. *)

let fmt6 v =
  let w = if abs_float v < 5e-7 then 0.0 else v in
  Printf.sprintf "%.6f" (w +. 0.0)

let canon v = if abs_float v < 5e-7 then 0.0 else v
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
  let next_tok () =
    let v = toks.(!pos) in
    incr pos; v
  in
  let op = next_tok () in
  let fun_ = next_tok () in
  let p =
    if fun_ = "POLY" then (
      let d = int_of_string (next_tok ()) in
      Array.init (d + 1) (fun _ -> float_of_string (next_tok ())))
    else if fun_ = "POW" then Array.init 4 (fun _ -> float_of_string (next_tok ()))
    else Array.init 3 (fun _ -> float_of_string (next_tok ()))
  in
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
  let tx rule t =
    if rule = "SEMIINF_DE" then (
      let x = exp (pi *. sinh t) in
      (x, x *. pi *. cosh t))
    else if rule = "SEMIINF_TANH" then (
      let x = exp t in
      (x, x))
    else if rule = "INF_TANH" then (sinh t, cosh t)
    else (
      let x = exp (t -. exp (-.t)) in
      (x, x *. (1.0 +. exp (-.t))))
  in
  let traprange rule lo hi l sgn =
    let g t =
      let x, dxdt = tx rule t in
      f (sgn *. x) *. dxdt
    in
    let n = ref 0 and s = ref 0.0 in
    for _m = 0 to l - 1 do
      incr n;
      if !n = 1 then s := 0.5 *. (hi -. lo) *. (g lo +. g hi)
      else (
        let it = ref 1 in
        for _j = 1 to !n - 2 do
          it := !it * 2
        done;
        let tnm = float !it in
        let del = (hi -. lo) /. tnm in
        let x = ref (lo +. 0.5 *. del) and sum = ref 0.0 in
        for _j = 0 to !it - 1 do
          sum := !sum +. g !x;
          x := !x +. del
        done;
        s := 0.5 *. (!s +. (hi -. lo) *. !sum /. tnm))
    done;
    !s
  in
  let ans = ref 0.0 in
  (if op = "DE_FINITE" then (
     let a = float_of_string (next_tok ()) in
     let b = float_of_string (next_tok ()) in
     let hmax = float_of_string (next_tok ()) in
     let l = int_of_string (next_tok ()) in
     let s = ref 0.0 in
     for n = 1 to l do
       if n = 1 then s := hmax *. 2.0 *. (b -. a) *. 0.25 *. f (0.5 *. (b +. a))
       else (
         let it = ref 1 in
         for _j = 1 to n - 2 do
           it := !it * 2
         done;
         let twoh = hmax /. float !it in
         let t = ref (0.5 *. twoh) and sum = ref 0.0 in
         for _j = 0 to !it - 1 do
           let q = exp (-2.0 *. sinh !t) in
           let del = (b -. a) *. q /. (1.0 +. q) in
           let fact = q /. ((1.0 +. q) *. (1.0 +. q)) *. cosh !t in
           sum := !sum +. fact *. (f (a +. del) +. f (b -. del));
           t := !t +. twoh
         done;
         s := 0.5 *. !s +. (b -. a) *. twoh *. !sum)
     done;
     ans := !s)
   else (
     let t0 = float_of_string (next_tok ()) in
     let t1 = float_of_string (next_tok ()) in
     let l = int_of_string (next_tok ()) in
     ans :=
       (if op = "INF_MIXED" then
          traprange "SEMIINF_MIXED" t0 t1 l 1.0 +. traprange "SEMIINF_MIXED" t0 t1 l (-1.0)
        else traprange op t0 t1 l 1.0)));
  print_endline (fmt6 (canon !ans))
