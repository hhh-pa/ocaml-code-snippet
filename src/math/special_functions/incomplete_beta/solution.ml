(* Incomplete beta function (NR 6.4). *)

let fmt6 v =
  let w = if abs_float v < 5e-7 then 0.0 else v in
  Printf.sprintf "%.6f" (w +. 0.0)

let canon v = if abs_float v < 5e-7 then 0.0 else v

let gcof =
  [| 57.1562356658629235; -59.5979603554754912; 14.1360979747417471;
     -0.491913816097620199; 0.339946499848118887e-4; 0.465236289270485756e-4;
     -0.983744753048795646e-4; 0.158088703224912494e-3; -0.210264441724104883e-3;
     0.217439618115212643e-3; -0.164318106536763890e-3; 0.844182239838527433e-4;
     -0.261908384015814087e-4; 0.368991826595316234e-5 |]

let gammln xx =
  let y = ref xx in
  let x = xx in
  let tmp = ref (x +. 5.2421875000000000) in
  tmp := (x +. 0.5) *. log !tmp -. !tmp;
  let ser = ref 0.999999999999997092 in
  for j = 0 to 13 do
    y := !y +. 1.0;
    ser := !ser +. gcof.(j) /. !y
  done;
  !tmp +. log (2.5066282746310005 *. !ser /. x)

let eps = 2.220446049250313e-16
let fpmin = 1.0020841800044864e-292

let betacf a b x =
  let qab = a +. b and qap = a +. 1.0 and qam = a -. 1.0 in
  let c = ref 1.0 in
  let d = ref (1.0 -. qab *. x /. qap) in
  if abs_float !d < fpmin then d := fpmin;
  d := 1.0 /. !d;
  let h = ref !d in
  (try
     for m = 1 to 9999 do
       let m2 = 2 * m in
       let aa =
         float m *. (b -. float m) *. x /. ((qam +. float m2) *. (a +. float m2))
       in
       d := 1.0 +. aa *. !d;
       if abs_float !d < fpmin then d := fpmin;
       c := 1.0 +. aa /. !c;
       if abs_float !c < fpmin then c := fpmin;
       d := 1.0 /. !d;
       h := !h *. !d *. !c;
       let aa =
         -.(a +. float m) *. (qab +. float m) *. x /. ((a +. float m2) *. (qap +. float m2))
       in
       d := 1.0 +. aa *. !d;
       if abs_float !d < fpmin then d := fpmin;
       c := 1.0 +. aa /. !c;
       if abs_float !c < fpmin then c := fpmin;
       d := 1.0 /. !d;
       let del = !d *. !c in
       h := !h *. del;
       if abs_float (del -. 1.0) <= eps then raise Exit
     done
   with Exit -> ());
  !h

let rec betai a b x =
  if x = 0.0 || x = 1.0 then x
  else (
    let bt = exp (gammln (a +. b) -. gammln a -. gammln b +. a *. log x +. b *. log (1.0 -. x)) in
    if x < (a +. 1.0) /. (a +. b +. 2.0) then bt *. betacf a b x /. a
    else 1.0 -. bt *. betacf b a (1.0 -. x) /. b)

and invbetai p a b =
  let epsi = 1.0e-8 in
  if p <= 0.0 then 0.0
  else if p >= 1.0 then 1.0
  else (
    let a1 = a -. 1.0 and b1 = b -. 1.0 in
    let x = ref 0.0 in
    (if a >= 1.0 && b >= 1.0 then (
       let pp = if p < 0.5 then p else 1.0 -. p in
       let t = sqrt (-2.0 *. log pp) in
       x := (2.30753 +. t *. 0.27061) /. (1.0 +. t *. (0.99229 +. t *. 0.04481)) -. t;
       if p < 0.5 then x := -. !x;
       let al = (!x *. !x -. 3.0) /. 6.0 in
       let h = 2.0 /. (1.0 /. (2.0 *. a -. 1.0) +. 1.0 /. (2.0 *. b -. 1.0)) in
       let w =
         (!x *. sqrt (al +. h) /. h)
         -. (1.0 /. (2.0 *. b -. 1.0) -. 1.0 /. (2.0 *. a -. 1.0))
            *. (al +. 5.0 /. 6.0 -. 2.0 /. (3.0 *. h))
       in
       x := a /. (a +. b *. exp (2.0 *. w)))
     else (
       let lna = log (a /. (a +. b)) in
       let lnb = log (b /. (a +. b)) in
       let t = exp (a *. lna) /. a in
       let u = exp (b *. lnb) /. b in
       let w = t +. u in
       x := if p < t /. w then (a *. w *. p) ** (1.0 /. a) else 1.0 -. (b *. w *. (1.0 -. p)) ** (1.0 /. b)));
    let afac = -.gammln a -. gammln b +. gammln (a +. b) in
    (try
       for j = 0 to 9 do
         if !x = 0.0 || !x = 1.0 then raise Exit;
         let err = betai a b !x -. p in
         let t = exp (a1 *. log !x +. b1 *. log (1.0 -. !x) +. afac) in
         let u = err /. t in
         let uu0 = u *. (a1 /. !x -. b1 /. (1.0 -. !x)) in
         let uu = if uu0 > 1.0 then 1.0 else uu0 in
         let tt = u /. (1.0 -. 0.5 *. uu) in
         x := !x -. tt;
         if !x <= 0.0 then x := 0.5 *. (!x +. tt);
         if !x >= 1.0 then x := 0.5 *. (!x +. tt +. 1.0);
         if abs_float tt < epsi *. !x && j > 0 then raise Exit
       done
     with Exit -> ());
    !x)

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
  let ans = ref 0.0 in
  (if op = "BETAI" then (
     let a = float_of_string (next_tok ()) in
     let b = float_of_string (next_tok ()) in
     let x = float_of_string (next_tok ()) in
     ans := betai a b x)
   else (
     let p = float_of_string (next_tok ()) in
     let a = float_of_string (next_tok ()) in
     let b = float_of_string (next_tok ()) in
     ans := invbetai p a b));
  print_endline (fmt6 (canon !ans))
