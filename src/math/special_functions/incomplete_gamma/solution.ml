(* Incomplete gamma and error functions (NR 6.2). *)

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

let erfcof =
  [| -1.3026537197817094; 6.4196979235649026e-1; 1.9476473204185836e-2;
     -9.561514786808631e-3; -9.46595344482036e-4; 3.66839497852761e-4;
     4.2523324806907e-5; -2.0278578112534e-5; -1.624290004647e-6;
     1.303655835580e-6; 1.5626441722e-8; -8.5238095915e-8;
     6.529054439e-9; 5.059343495e-9; -9.91364156e-10;
     -2.27365122e-10; 9.6467911e-11; 2.394038e-12;
     -6.886027e-12; 8.94487e-13; 3.13092e-13;
     -1.12708e-13; 3.81e-16; 7.106e-15;
     -1.523e-15; -9.4e-17; 1.21e-16; -2.8e-17 |]

let erfccheb z =
  let t = 2.0 /. (2.0 +. z) in
  let ty = 4.0 *. t -. 2.0 in
  let d = ref 0.0 and dd = ref 0.0 in
  for j = 27 downto 1 do
    let tmp = !d in
    d := ty *. !d -. !dd +. erfcof.(j);
    dd := tmp
  done;
  t *. exp (-.z *. z +. 0.5 *. (erfcof.(0) +. ty *. !d) -. !dd)

let erfcx x = if x >= 0.0 then erfccheb x else 2.0 -. erfccheb (-.x)

let inverfc p =
  if p >= 2.0 then -100.0
  else if p <= 0.0 then 100.0
  else (
    let pp = if p < 1.0 then p else 2.0 -. p in
    let t = sqrt (-2.0 *. log (pp /. 2.0)) in
    let x =
      ref (-0.70711 *. ((2.30753 +. t *. 0.27061) /. (1.0 +. t *. (0.99229 +. t *. 0.04481)) -. t))
    in
    for _j = 0 to 1 do
      let err = erfcx !x -. pp in
      x := !x +. err /. (1.12837916709551257 *. exp (-. !x *. !x) -. !x *. err)
    done;
    if p < 1.0 then !x else -. !x)

let eps = 2.220446049250313e-16
let fpmin = 1.0020841800044864e-292

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
  (if op = "GAMMP" || op = "GAMMQ" then (
     let a = float_of_string (next_tok ()) in
     let x = float_of_string (next_tok ()) in
     let gln = gammln a in
     if x = 0.0 then ans := if op = "GAMMP" then 0.0 else 1.0
     else if x < a +. 1.0 then (
       let ap = ref a and del = ref (1.0 /. a) and sum = ref (1.0 /. a) in
       (try
          for _n = 0 to 999 do
            ap := !ap +. 1.0;
            del := !del *. x /. !ap;
            sum := !sum +. !del;
            if abs_float !del < abs_float !sum *. eps then raise Exit
          done
        with Exit -> ());
       let p = !sum *. exp (-.x +. a *. log x -. gln) in
       ans := if op = "GAMMP" then p else 1.0 -. p)
     else (
       let b = ref (x +. 1.0 -. a) and c = ref (1.0 /. fpmin) and d = ref (1.0 /. (x +. 1.0 -. a)) in
       let h = ref !d in
       (try
          for i = 1 to 1000 do
            let an = -.float i *. (float i -. a) in
            b := !b +. 2.0;
            d := an *. !d +. !b;
            if abs_float !d < fpmin then d := fpmin;
            c := !b +. an /. !c;
            if abs_float !c < fpmin then c := fpmin;
            d := 1.0 /. !d;
            let del = !d *. !c in
            h := !h *. del;
            if abs_float (del -. 1.0) <= eps then raise Exit
          done
        with Exit -> ());
       let q = exp (-.x +. a *. log x -. gln) *. !h in
       ans := if op = "GAMMP" then 1.0 -. q else q))
   else if op = "ERF" then (
     let x = float_of_string (next_tok ()) in
     ans := if x >= 0.0 then 1.0 -. erfccheb x else erfccheb (-.x) -. 1.0)
   else if op = "ERFC" then ans := erfcx (float_of_string (next_tok ()))
   else if op = "INVERFC" then ans := inverfc (float_of_string (next_tok ()))
   else ans := inverfc (1.0 -. float_of_string (next_tok ())));
  print_endline (fmt6 (canon !ans))
