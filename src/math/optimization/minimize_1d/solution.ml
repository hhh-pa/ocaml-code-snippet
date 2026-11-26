(* One-dimensional minimization (NR 10.1-10.4): bracketing
   (mnbrak), golden section search, Brent's method, and
   derivative-using dBrent.
   Reference: Press et al., Numerical Recipes 3e, Sections 10.1-10.4.
   No online-judge problem found -- fixtures are self-generated
   (scipy oracle), no header link. *)

let fmt6 v =
  let w = if abs_float v < 5e-7 then 0.0 else v in
  Printf.sprintf "%.6f" (w +. 0.0)

let canon v = if abs_float v < 5e-7 then 0.0 else v

let fn = ref ""
let fp = ref [||]

let func t =
  if !fn = "POLY" then (
    let s = ref 0.0 in
    for i = Array.length !fp - 1 downto 0 do
      s := !s *. t +. !fp.(i)
    done;
    !s)
  else if !fn = "TRIG" then !fp.(0) *. sin (!fp.(1) *. t +. !fp.(2))
  else !fp.(0) *. ((!fp.(1) *. t +. !fp.(2)) ** !fp.(3))

let funcd t =
  if !fn = "POLY" then (
    let s = ref 0.0 in
    for i = Array.length !fp - 1 downto 1 do
      s := !s *. t +. float i *. !fp.(i)
    done;
    !s)
  else if !fn = "TRIG" then !fp.(0) *. !fp.(1) *. cos (!fp.(1) *. t +. !fp.(2))
  else !fp.(0) *. !fp.(3) *. !fp.(1) *. ((!fp.(1) *. t +. !fp.(2)) ** (!fp.(3) -. 1.0))

let sgn a b = if b >= 0.0 then abs_float a else -. abs_float a

let ax = ref 0.0
and bx = ref 0.0
and cx = ref 0.0
and fa = ref 0.0
and fb = ref 0.0
and fc = ref 0.0

let bracket a b =
  let gold = 1.618034 and glimit = 100.0 and tiny = 1.0e-20 in
  ax := a;
  bx := b;
  fa := func !ax;
  fb := func !bx;
  (if !fb > !fa then (
     let tmp = !ax in
     ax := !bx;
     bx := tmp;
     let tmp = !fb in
     fb := !fa;
     fa := tmp));
  cx := !bx +. gold *. (!bx -. !ax);
  fc := func !cx;
  let rec loop () =
    if !fb > !fc then (
      let r = (!bx -. !ax) *. (!fb -. !fc) in
      let q = (!bx -. !cx) *. (!fb -. !fa) in
      let u =
        ref
          (!bx
          -. ((!bx -. !cx) *. q -. (!bx -. !ax) *. r)
             /. (2.0 *. sgn (max (abs_float (q -. r)) tiny) (q -. r)))
      in
      let ulim = !bx +. glimit *. (!cx -. !bx) in
      let fu = ref 0.0 in
      let done_ = ref false in
      (if (!bx -. !u) *. (!u -. !cx) > 0.0 then (
         fu := func !u;
         if !fu < !fc then (
           ax := !bx;
           bx := !u;
           fa := !fb;
           fb := !fu;
           done_ := true)
         else if !fu > !fb then (
           cx := !u;
           fc := !fu;
           done_ := true)
         else (
           u := !cx +. gold *. (!cx -. !bx);
           fu := func !u))
       else if (!cx -. !u) *. (!u -. ulim) > 0.0 then (
         fu := func !u;
         if !fu < !fc then (
           let nu = !u +. gold *. (!u -. !cx) in
           let nfu = func nu in
           bx := !cx;
           cx := !u;
           u := nu;
           fb := !fc;
           fc := !fu;
           fu := nfu))
       else if (!u -. ulim) *. (ulim -. !cx) >= 0.0 then (
         u := ulim;
         fu := func !u)
       else (
         u := !cx +. gold *. (!cx -. !bx);
         fu := func !u));
      if not !done_ then (
        ax := !bx;
        bx := !cx;
        cx := !u;
        fa := !fb;
        fb := !fc;
        fc := !fu;
        loop ()))
  in
  loop ()

let golden tol =
  let r = 0.61803399 and c = 1.0 -. 0.61803399 in
  let x0 = ref !ax and x3 = ref !cx in
  let x1 = ref 0.0 and x2 = ref 0.0 in
  (if abs_float (!cx -. !bx) > abs_float (!bx -. !ax) then (
     x1 := !bx;
     x2 := !bx +. c *. (!cx -. !bx))
   else (
     x2 := !bx;
     x1 := !bx -. c *. (!bx -. !ax)));
  let f1 = ref (func !x1) and f2 = ref (func !x2) in
  let rec loop () =
    if abs_float (!x3 -. !x0) > tol *. (abs_float !x1 +. abs_float !x2) then (
      (if !f2 < !f1 then (
         let nx = r *. !x2 +. c *. !x3 in
         x0 := !x1;
         x1 := !x2;
         x2 := nx;
         f1 := !f2;
         f2 := func !x2)
       else (
         let nx = r *. !x1 +. c *. !x0 in
         x3 := !x2;
         x2 := !x1;
         x1 := nx;
         f2 := !f1;
         f1 := func !x1));
      loop ())
  in
  loop ();
  if !f1 < !f2 then (!x1, !f1) else (!x2, !f2)

exception TooMany
exception DbrentDone

let brent tol =
  let itmax = 100 in
  let cgold = 0.3819660 in
  let zeps = 2.220446049250313e-16 *. 1.0e-3 in
  let a = ref (if !ax < !cx then !ax else !cx)
  and b = ref (if !ax > !cx then !ax else !cx) in
  let x = ref !bx and w = ref !bx and v = ref !bx in
  let fx = ref (func !x) in
  let fw = ref !fx in
  let fv = ref !fx in
  let d = ref 0.0 and e = ref 0.0 in
  let rec loop iter =
    if iter >= itmax then raise TooMany
    else (
      let xm = 0.5 *. (!a +. !b) in
      let tol1 = tol *. abs_float !x +. zeps in
      let tol2 = 2.0 *. tol1 in
      if abs_float (!x -. xm) <= tol2 -. 0.5 *. (!b -. !a) then (!x, !fx)
      else (
        (if abs_float !e > tol1 then (
           let r = (!x -. !w) *. (!fx -. !fv) in
           let q0 = (!x -. !v) *. (!fx -. !fw) in
           let p = ref ((!x -. !v) *. q0 -. (!x -. !w) *. r) in
           let q = ref (2.0 *. (q0 -. r)) in
           (if !q > 0.0 then p := -. !p);
           q := abs_float !q;
           let etemp = !e in
           e := !d;
           if abs_float !p >= abs_float (0.5 *. !q *. etemp) || !p <= !q *. (!a -. !x)
              || !p >= !q *. (!b -. !x)
           then (
             e := (if !x >= xm then !a -. !x else !b -. !x);
             d := cgold *. !e)
           else (
             d := !p /. !q;
             let u = !x +. !d in
             if u -. !a < tol2 || !b -. u < tol2 then d := sgn tol1 (xm -. !x)))
         else (
           e := (if !x >= xm then !a -. !x else !b -. !x);
           d := cgold *. !e));
        let u = if abs_float !d >= tol1 then !x +. !d else !x +. sgn tol1 !d in
        let fu = func u in
        (if fu <= !fx then (
           (if u >= !x then a := !x else b := !x);
           v := !w;
           w := !x;
           x := u;
           fv := !fw;
           fw := !fx;
           fx := fu)
         else (
           (if u < !x then a := u else b := u);
           if fu <= !fw || !w = !x then (
             v := !w;
             w := u;
             fv := !fw;
             fw := fu)
           else if fu <= !fv || !v = !x || !v = !w then (
             v := u;
             fv := fu)));
        loop (iter + 1)))
  in
  loop 0

let dbrent tol =
  let itmax = 100 in
  let zeps = 2.220446049250313e-16 *. 1.0e-3 in
  let a = ref (if !ax < !cx then !ax else !cx)
  and b = ref (if !ax > !cx then !ax else !cx) in
  let x = ref !bx and w = ref !bx and v = ref !bx in
  let fx = ref (func !x) in
  let fw = ref !fx in
  let fv = ref !fx in
  let dx = ref (funcd !x) in
  let dw = ref !dx in
  let dv = ref !dx in
  let d = ref 0.0 and e = ref 0.0 in
  let rec loop iter =
    if iter >= itmax then raise TooMany
    else (
      let xm = 0.5 *. (!a +. !b) in
      let tol1 = tol *. abs_float !x +. zeps in
      let tol2 = 2.0 *. tol1 in
      if abs_float (!x -. xm) <= tol2 -. 0.5 *. (!b -. !a) then (!x, !fx)
      else (
        (if abs_float !e > tol1 then (
           let d1 = ref (2.0 *. (!b -. !a)) in
           let d2 = ref !d1 in
           (if !dw <> !dx then d1 := (!w -. !x) *. !dx /. (!dx -. !dw));
           (if !dv <> !dx then d2 := (!v -. !x) *. !dx /. (!dx -. !dv));
           let u1 = !x +. !d1 and u2 = !x +. !d2 in
           let ok1 = (!a -. u1) *. (u1 -. !b) > 0.0 && !dx *. !d1 <= 0.0 in
           let ok2 = (!a -. u2) *. (u2 -. !b) > 0.0 && !dx *. !d2 <= 0.0 in
           let olde = !e in
           e := !d;
           if ok1 || ok2 then (
             (if ok1 && ok2 then d := (if abs_float !d1 < abs_float !d2 then !d1 else !d2)
              else if ok1 then d := !d1
              else d := !d2);
             if abs_float !d <= abs_float (0.5 *. olde) then (
               let u = !x +. !d in
               if u -. !a < tol2 || !b -. u < tol2 then d := sgn tol1 (xm -. !x))
             else (
               e := (if !dx >= 0.0 then !a -. !x else !b -. !x);
               d := 0.5 *. !e))
           else (
             e := (if !dx >= 0.0 then !a -. !x else !b -. !x);
             d := 0.5 *. !e))
         else (
           e := (if !dx >= 0.0 then !a -. !x else !b -. !x);
           d := 0.5 *. !e));
        let u, fu =
          if abs_float !d >= tol1 then (
            let u = !x +. !d in
            (u, func u))
          else (
            let u = !x +. sgn tol1 !d in
            let fu = func u in
            if fu > !fx then raise DbrentDone else (u, fu))
        in
        let du = funcd u in
        (if fu <= !fx then (
           (if u >= !x then a := !x else b := !x);
           v := !w;
           fv := !fw;
           dv := !dw;
           w := !x;
           fw := !fx;
           dw := !dx;
           x := u;
           fx := fu;
           dx := du)
         else (
           (if u < !x then a := u else b := u);
           if fu <= !fw || !w = !x then (
             v := !w;
             fv := !fw;
             dv := !dw;
             w := u;
             fw := fu;
             dw := du)
           else if fu < !fv || !v = !x || !v = !w then (
             v := u;
             fv := fu;
             dv := du)));
        loop (iter + 1)))
  in
  try loop 0 with DbrentDone -> (!x, !fx)

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
  let op = toks.(0) in
  fn := toks.(1);
  let p = ref 2 in
  (if !fn = "POLY" then (
     let d = int_of_string toks.(2) in
     fp := Array.init (d + 1) (fun i -> float_of_string toks.(3 + i));
     p := 3 + d + 1)
   else if !fn = "POW" then (
     fp := Array.init 4 (fun i -> float_of_string toks.(2 + i));
     p := 6)
   else (
     fp := Array.init 3 (fun i -> float_of_string toks.(2 + i));
     p := 5));
  let a = float_of_string toks.(!p) and b = float_of_string toks.(!p + 1) in
  if op = "BRACKET" then (
    bracket a b;
    print_endline
      (fmt6 !ax ^ " " ^ fmt6 !bx ^ " " ^ fmt6 !cx ^ " " ^ fmt6 (canon !fa) ^ " "
     ^ fmt6 (canon !fb) ^ " " ^ fmt6 (canon !fc)))
  else if op = "GOLDEN" || op = "BRENT" || op = "DBRENT" then (
    let tol = float_of_string toks.(!p + 2) in
    bracket a b;
    if op = "GOLDEN" then (
      let xmin, fmin = golden tol in
      print_endline (fmt6 (canon xmin) ^ " " ^ fmt6 (canon fmin) ^ " 0"))
    else (
      try
        let xmin, fmin = if op = "BRENT" then brent tol else dbrent tol in
        print_endline (fmt6 (canon xmin) ^ " " ^ fmt6 (canon fmin) ^ " 0")
      with TooMany -> print_endline "0.000000 0.000000 1"))
