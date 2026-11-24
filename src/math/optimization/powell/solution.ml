(* Multidimensional minimization by Powell's direction set
   method (with Brent line searches).
   Reference: Press et al., Numerical Recipes 3e, Sections 10.6-10.7.
   No online-judge problem found -- fixtures are self-generated
   (scipy oracle), no header link. *)

let fmt6 v =
  let w = if abs_float v < 5e-7 then 0.0 else v in
  Printf.sprintf "%.6f" (w +. 0.0)

let canon v = if abs_float v < 5e-7 then 0.0 else v

let fn = ref ""
let fp = ref [||]

let func x =
  if !fn = "QUAD" then (
    let n = Array.length x in
    let s = ref !fp.(n) in
    for i = 0 to n - 1 do
      s := !s +. (x.(i) -. !fp.(i)) *. (x.(i) -. !fp.(i))
    done;
    !s)
  else (
    let n = Array.length x in
    let s = ref 0.0 in
    for i = 0 to n - 2 do
      let a = x.(i + 1) -. x.(i) *. x.(i) in
      let b = 1.0 -. x.(i) in
      s := !s +. 100.0 *. a *. a +. b *. b
    done;
    !s)

let sgn a b = if b >= 0.0 then abs_float a else -. abs_float a

exception TooMany

let bracket1d f a b =
  let gold = 1.618034 and glimit = 100.0 and tiny = 1.0e-20 in
  let ax = ref a and bx = ref b in
  let fa = ref (f !ax) and fb = ref (f !bx) in
  (if !fb > !fa then (
     let tmp = !ax in
     ax := !bx;
     bx := tmp;
     let tmp = !fb in
     fb := !fa;
     fa := tmp));
  let cx = ref (!bx +. gold *. (!bx -. !ax)) in
  let fc = ref (f !cx) in
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
      let ret = ref None in
      (if (!bx -. !u) *. (!u -. !cx) > 0.0 then (
         fu := f !u;
         if !fu < !fc then (
           ax := !bx;
           bx := !u;
           fa := !fb;
           fb := !fu;
           ret := Some ())
         else if !fu > !fb then (
           cx := !u;
           fc := !fu;
           ret := Some ())
         else (
           u := !cx +. gold *. (!cx -. !bx);
           fu := f !u))
       else if (!cx -. !u) *. (!u -. ulim) > 0.0 then (
         fu := f !u;
         if !fu < !fc then (
           let nu = !u +. gold *. (!u -. !cx) in
           let nfu = f nu in
           bx := !cx;
           cx := !u;
           u := nu;
           fb := !fc;
           fc := !fu;
           fu := nfu))
       else if (!u -. ulim) *. (ulim -. !cx) >= 0.0 then (
         u := ulim;
         fu := f !u)
       else (
         u := !cx +. gold *. (!cx -. !bx);
         fu := f !u));
      match !ret with
      | Some () -> ()
      | None ->
          ax := !bx;
          bx := !cx;
          cx := !u;
          fa := !fb;
          fb := !fc;
          fc := !fu;
          loop ())
  in
  loop ();
  (!ax, !bx, !cx)

let brent1d f ax bx cx tol =
  let itmax = 100 in
  let cgold = 0.3819660 in
  let zeps = 2.220446049250313e-16 *. 1.0e-3 in
  let a = ref (if ax < cx then ax else cx)
  and b = ref (if ax > cx then ax else cx) in
  let x = ref bx and w = ref bx and v = ref bx in
  let fx = ref (f !x) in
  let fw = ref !fx in
  let fv = ref !fx in
  let d = ref 0.0 and e = ref 0.0 in
  let fmin = ref 0.0 in
  let rec loop iter =
    if iter >= itmax then raise TooMany
    else (
      let xm = 0.5 *. (!a +. !b) in
      let tol1 = tol *. abs_float !x +. zeps in
      let tol2 = 2.0 *. tol1 in
      if abs_float (!x -. xm) <= tol2 -. 0.5 *. (!b -. !a) then (
        fmin := !fx;
        !x)
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
        let fu = f u in
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
  let xmin = loop 0 in
  (xmin, !fmin)

let minimize point ftol =
  let itmax = 200 in
  let tiny = 1.0e-25 in
  let n = Array.length point in
  let ximat = Array.make_matrix n n 0.0 in
  for i = 0 to n - 1 do
    ximat.(i).(i) <- 1.0
  done;
  let p = Array.copy point in
  let pt = Array.copy point in
  let ptt = Array.make n 0.0 in
  let xi = Array.make n 0.0 in
  let fret = ref (func p) in
  let iter = ref 0 in
  let linmin () =
    let pc = Array.copy p and xic = Array.copy xi in
    let f1 x =
      let xt = Array.make n 0.0 in
      for j = 0 to n - 1 do
        xt.(j) <- pc.(j) +. x *. xic.(j)
      done;
      func xt
    in
    let ax, bx, cx = bracket1d f1 0.0 1.0 in
    let xmin, fmin = brent1d f1 ax bx cx 3.0e-8 in
    for j = 0 to n - 1 do
      xi.(j) <- xi.(j) *. xmin;
      p.(j) <- p.(j) +. xi.(j)
    done;
    fmin
  in
  let rec loop () =
    let fp = !fret in
    let ibig = ref 0 and del = ref 0.0 in
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        xi.(j) <- ximat.(j).(i)
      done;
      let fptt = !fret in
      fret := linmin ();
      if fptt -. !fret > !del then (
        del := fptt -. !fret;
        ibig := i + 1)
    done;
    if 2.0 *. (fp -. !fret) <= ftol *. (abs_float fp +. abs_float !fret) +. tiny then
      (p, !fret, !iter)
    else (
      (if !iter = itmax then raise TooMany);
      for j = 0 to n - 1 do
        ptt.(j) <- 2.0 *. p.(j) -. pt.(j);
        xi.(j) <- p.(j) -. pt.(j);
        pt.(j) <- p.(j)
      done;
      let fptt = func ptt in
      (if fptt < fp then (
         let d1 = fp -. !fret -. !del in
         let sq1 = d1 *. d1 in
         let d2 = fp -. fptt in
         let sq2 = d2 *. d2 in
         let t = 2.0 *. (fp -. 2.0 *. !fret +. fptt) *. sq1 -. !del *. sq2 in
         if t < 0.0 then (
           fret := linmin ();
           for j = 0 to n - 1 do
             ximat.(j).(!ibig - 1) <- ximat.(j).(n - 1);
             ximat.(j).(n - 1) <- xi.(j)
           done)));
      incr iter;
      loop ())
  in
  try
    let pmin, fr, it = loop () in
    (pmin, fr, it, 0)
  with TooMany -> (p, !fret, !iter, 1)

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
  let n = int_of_string toks.(2) in
  let p = ref 3 in
  (if !fn = "QUAD" then (
     fp := Array.init (n + 1) (fun i -> float_of_string toks.(3 + i));
     p := 3 + n + 1));
  let point = Array.init n (fun i -> float_of_string toks.(!p + i)) in
  let ftol = float_of_string toks.(!p + n) in
  if op = "POWELL" then (
    let pmin, fret, it, flag = minimize point ftol in
    if flag <> 0 then (
      let parts = Array.to_list (Array.init n (fun _ -> "0.000000")) in
      print_endline (String.concat " " parts ^ " 0.000000 " ^ string_of_int it ^ " 1"))
    else (
      let parts = Array.to_list (Array.init n (fun i -> fmt6 (canon pmin.(i)))) in
      print_endline
        (String.concat " " parts ^ " " ^ fmt6 (canon fret) ^ " " ^ string_of_int it ^ " 0")))
