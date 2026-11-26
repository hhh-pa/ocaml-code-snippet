(* Fresnel and cosine/sine integrals (NR 6.8). *)

let fmt6 v =
  let w = if abs_float v < 5e-7 then 0.0 else v in
  Printf.sprintf "%.6f" (w +. 0.0)

let canon v = if abs_float v < 5e-7 then 0.0 else v
let pi = 3.141592653589793

let cxadd (ar, ai) (br, bi) = (ar +. br, ai +. bi)
let cxsub (ar, ai) (br, bi) = (ar -. br, ai -. bi)

let cxmul (ar, ai) (br, bi) =
  (ar *. br -. ai *. bi, ar *. bi +. ai *. br)

let cxdiv (ar, ai) (br, bi) =
  let d = br *. br +. bi *. bi in
  ((ar *. br +. ai *. bi) /. d, (ai *. br -. ar *. bi) /. d)

let frenel x =
  let maxit = 100 in
  let piby2 = pi /. 2.0 in
  let xmin = 1.5 in
  let eps = 2.220446049250313e-16 in
  let fpmin = 2.2250738585072014e-308 in
  let big = 1.7976931348623157e308 *. eps in
  let ax = abs_float x in
  let cs = ref (0.0, 0.0) in
  (if ax < sqrt fpmin then cs := (ax, 0.0)
   else if ax <= xmin then (
     let sum = ref 0.0 and sums = ref 0.0 and sumc = ref ax and sign = ref 1.0 in
     let fact = piby2 *. ax *. ax in
     let odd = ref true and term = ref ax and n = ref 3 in
     (try
        for k = 1 to maxit do
          term := !term *. fact /. float k;
          sum := !sum +. !sign *. !term /. float !n;
          let test = abs_float !sum *. eps in
          (if !odd then (
             sign := -. !sign;
             sums := !sum;
             sum := !sumc)
           else (
             sumc := !sum;
             sum := !sums));
          if !term < test then raise Exit;
          odd := not !odd;
          n := !n + 2
        done
      with Exit -> ());
     cs := (!sumc, !sums))
   else (
     let pix2 = pi *. ax *. ax in
     let b = ref (1.0, -.pix2) and cc = ref (big, 0.0) in
     let d = ref (cxdiv (1.0, 0.0) !b) and h = ref !(ref (cxdiv (1.0, 0.0) !b)) in
     let n = ref (-1) in
     (try
        for k = 2 to maxit do
          n := !n + 2;
          let a = -.float !n *. (float !n +. 1.0) in
          b := (fst !b +. 4.0, snd !b);
          d := cxdiv (1.0, 0.0) (cxadd (a *. fst !d, a *. snd !d) !b);
          cc := cxadd !b (cxdiv (a, 0.0) !cc);
          let del = cxmul !cc !d in
          h := cxmul !h del;
          if abs_float (fst del -. 1.0) +. abs_float (snd del) <= eps then raise Exit
        done
      with Exit -> ());
     h := cxmul !h (ax, -.ax);
     let e = (cos (0.5 *. pix2), sin (0.5 *. pix2)) in
     let t = cxsub (1.0, 0.0) (cxmul e !h) in
     cs := cxmul (0.5, 0.5) t));
  if x < 0.0 then cs := (-.fst !cs, -.snd !cs);
  !cs

let cisi x =
  let maxit = 100 in
  let euler = 0.577215664901533 in
  let piby2 = 1.570796326794897 in
  let tmin = 2.0 in
  let eps = 2.220446049250313e-16 in
  let fpmin = 2.2250738585072014e-308 *. 4.0 in
  let big = 1.7976931348623157e308 *. eps in
  let t = abs_float x in
  if t = 0.0 then (-.big, 0.0)
  else (
    let cs = ref (0.0, 0.0) in
    (if t > tmin then (
       let b = ref (1.0, t) and c = ref (big, 0.0) in
       let d = ref (cxdiv (1.0, 0.0) !b) and h = ref (cxdiv (1.0, 0.0) !b) in
       (try
          for i = 1 to maxit - 1 do
            let a = -.float i *. float i in
            b := (fst !b +. 2.0, snd !b);
            d := cxdiv (1.0, 0.0) (cxadd (a *. fst !d, a *. snd !d) !b);
            c := cxadd !b (cxdiv (a, 0.0) !c);
            let del = cxmul !c !d in
            h := cxmul !h del;
            if abs_float (fst del -. 1.0) +. abs_float (snd del) <= eps then raise Exit
          done
        with Exit -> ());
       h := cxmul (cos t, -.sin t) !h;
       cs := (-.fst !h, snd !h +. piby2))
     else (
       let sumc = ref 0.0 and sums = ref 0.0 in
       (if t < sqrt fpmin then (
          sumc := 0.0;
          sums := t)
        else (
          let sum = ref 0.0 and sign = ref 1.0 and fact = ref 1.0 in
          let sc = ref 0.0 and ss = ref 0.0 and odd = ref true in
          (try
             for k = 1 to maxit do
               fact := !fact *. t /. float k;
               let term = !fact /. float k in
               sum := !sum +. !sign *. term;
               let err = term /. abs_float !sum in
               (if !odd then (
                  sign := -. !sign;
                  ss := !sum;
                  sum := !sc)
                else (
                  sc := !sum;
                  sum := !ss));
               if err < eps then raise Exit;
               odd := not !odd
             done
           with Exit -> ());
          sumc := !sc;
          sums := !ss));
       cs := (!sumc +. log t +. euler, !sums)));
    if x < 0.0 then cs := (fst !cs, -.snd !cs);
    !cs)

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
  let x = float_of_string toks.(1) in
  let r, im =
    if toks.(0) = "FRESNEL" then frenel x
    else
      (* NB: cisi returns [Ci Si]; keep token order explicit (OCaml
         arg evaluation order is unspecified). *)
      let v = cisi x in
      v
  in
  print_string (fmt6 (canon r));
  print_string " ";
  print_string (fmt6 (canon im));
  print_newline ()
