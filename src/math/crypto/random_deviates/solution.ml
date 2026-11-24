(* Non-uniform deviates (NR 7.3). *)

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
  let x = xx in
  let tmp = ref (x +. 5.2421875000000000) in
  tmp := (x +. 0.5) *. log !tmp -. !tmp;
  let ser = ref 0.999999999999997092 and y = ref xx in
  for j = 0 to 13 do
    y := !y +. 1.0;
    ser := !ser +. gcof.(j) /. !y
  done;
  !tmp +. log (2.5066282746310005 *. !ser /. x)

type ran = { mutable u : int64; mutable v : int64; mutable w : int64 }

let doubc = 5.42101086242752217E-20
let u64_dbl v = float_of_string (Printf.sprintf "%Lu" v)

let ran_step r =
  r.u <- Int64.add (Int64.mul r.u 2862933555777941757L) 7046029254386353087L;
  r.v <- Int64.logxor r.v (Int64.shift_right_logical r.v 17);
  r.v <- Int64.logxor r.v (Int64.shift_left r.v 31);
  r.v <- Int64.logxor r.v (Int64.shift_right_logical r.v 8);
  r.w <-
    Int64.add
      (Int64.mul 4294957665L (Int64.logand r.w 0xFFFFFFFFL))
      (Int64.shift_right_logical r.w 32);
  let x = ref (Int64.logxor r.u (Int64.shift_left r.u 21)) in
  x := Int64.logxor !x (Int64.shift_right_logical !x 35);
  x := Int64.logxor !x (Int64.shift_left !x 4);
  Int64.logxor (Int64.add !x r.v) r.w

let ran_make j =
  let r = { u = 0L; v = 4101842887655102017L; w = 1L } in
  r.u <- Int64.logxor j r.v;
  ignore (ran_step r);
  r.v <- r.u;
  ignore (ran_step r);
  r.w <- r.v;
  ignore (ran_step r);
  r

let ran_doub r = doubc *. u64_dbl (ran_step r)

let expon_dev r bet =
  let u = ref 0.0 and cont = ref true in
  while !cont do
    u := ran_doub r;
    if !u <> 0.0 then cont := false
  done;
  -.log !u /. bet

let logistic_dev r mu sg =
  let u = ref 0.0 and cont = ref true in
  while !cont do
    u := ran_doub r;
    if !u *. (1.0 -. !u) <> 0.0 then cont := false
  done;
  mu +. 0.551328895421792050 *. sg *. log (!u /. (1.0 -. !u))

let cauchy_dev r mu sg =
  let v1 = ref 0.0 and v2 = ref 0.0 and cont = ref true in
  while !cont do
    v1 := 2.0 *. ran_doub r -. 1.0;
    v2 := ran_doub r;
    if not (!v1 *. !v1 +. !v2 *. !v2 >= 1.0 || !v2 = 0.0) then cont := false
  done;
  mu +. sg *. !v1 /. !v2

let normal_dev r mu sg =
  let u = ref 0.0 and v = ref 0.0 and cont = ref true in
  while !cont do
    u := ran_doub r;
    v := 1.7156 *. (ran_doub r -. 0.5);
    let x = !u -. 0.449871 and y = abs_float !v +. 0.386595 in
    let q = x *. x +. y *. (0.19600 *. y -. 0.25472 *. x) in
    if not (q > 0.27597 && (q > 0.27846 || !v *. !v > -4.0 *. log !u *. !u *. !u)) then
      cont := false
  done;
  mu +. sg *. !v /. !u

type bm = { r : ran; mu : float; sg : float; mutable stored : float }

let bm_make mu sg j = { r = ran_make j; mu; sg; stored = 0.0 }

let bm_dev b =
  if b.stored = 0.0 then (
    let v1 = ref 0.0 and v2 = ref 0.0 and rsq = ref 0.0 and cont = ref true in
    while !cont do
      v1 := 2.0 *. ran_doub b.r -. 1.0;
      v2 := 2.0 *. ran_doub b.r -. 1.0;
      rsq := !v1 *. !v1 +. !v2 *. !v2;
      if not (!rsq >= 1.0 || !rsq = 0.0) then cont := false
    done;
    let fac = sqrt (-2.0 *. log !rsq /. !rsq) in
    b.stored <- !v1 *. fac;
    b.mu +. b.sg *. !v2 *. fac)
  else (
    let fac = b.stored in
    b.stored <- 0.0;
    b.mu +. b.sg *. fac)

type gam = { r : ran; alph : float; oalph : float; bet : float; a1 : float; a2 : float }

let gam_make aalph bbet j =
  let alph = if aalph < 1.0 then aalph +. 1.0 else aalph in
  let a1 = alph -. 1.0 /. 3.0 in
  { r = ran_make j; alph; oalph = aalph; bet = bbet; a1; a2 = 1.0 /. sqrt (9.0 *. a1) }

let gam_dev g =
  let u = ref 0.0 and v = ref 0.0 and x = ref 0.0 and cont = ref true in
  while !cont do
    let inner = ref true in
    while !inner do
      x := normal_dev g.r 0.0 1.0;
      v := 1.0 +. g.a2 *. !x;
      if !v > 0.0 then inner := false
    done;
    v := !v *. !v *. !v;
    u := ran_doub g.r;
    if
      not
        (!u > 1.0 -. 0.331 *. !x *. !x *. !x *. !x
        && log !u > 0.5 *. !x *. !x +. g.a1 *. (1.0 -. !v +. log !v))
    then cont := false
  done;
  if g.alph = g.oalph then g.a1 *. !v /. g.bet
  else (
    let cont2 = ref true in
    while !cont2 do
      u := ran_doub g.r;
      if !u <> 0.0 then cont2 := false
    done;
    (!u ** (1.0 /. g.oalph)) *. g.a1 *. !v /. g.bet)

type pois = { r : ran; lam : float }

let pois_dev d =
  let lam = d.lam in
  if lam < 5.0 then (
    let lamexp = exp (-.lam) in
    let k = ref (-1) and t = ref 1.0 and cont = ref true in
    while !cont do
      k := !k + 1;
      t := !t *. ran_doub d.r;
      if not (!t > lamexp) then cont := false
    done;
    !k)
  else (
    let sqlam = sqrt lam and loglam = log lam in
    let k = ref 0 and cont = ref true in
    while !cont do
      let u = 0.64 *. ran_doub d.r and v = -0.68 +. 1.28 *. ran_doub d.r in
      if u <> 0.0 then (
        let v2 = v *. v in
        let squeezed =
          lam > 13.5
          && (if v >= 0.0 then v2 > 6.5 *. u *. (0.64 -. u) *. (u +. 0.2)
              else v2 > 9.6 *. u *. (0.66 -. u) *. (u +. 0.07))
        in
        if not squeezed then (
          let kf = floor (sqlam *. (v /. u) +. lam +. 0.5) in
          if kf >= 0.0 && kf < 9.223372036854776e18 then (
            k := int_of_float kf;
            let u2 = u *. u in
            if lam > 13.5 && (if v >= 0.0 then v2 < 15.2 *. u2 *. (0.61 -. u) *. (0.8 -. u)
                              else v2 < 6.76 *. u2 *. (0.62 -. u) *. (1.4 -. u)) then
              cont := false
            else (
              let lfac = gammln (float !k +. 1.0) in
              let p = sqlam *. exp (-.lam +. float !k *. loglam -. lfac) in
              if u2 < p then cont := false))))
    done;
    !k)

type binom =
  { r : ran; pp : float; p : float; n : int; swch : int;
    pb : float; pbits : int array;
    cdf : float array;
    np : float; glnp : float; plog : float; pclog : float; sq : float;
    lfact : float array }

let binom_make nn pp j =
  let p = if pp <= 0.5 then pp else 1.0 -. pp in
  let b =
    { r = ran_make j; pp; p; n = nn; swch = 0;
      pb = p; pbits = Array.make 5 0;
      cdf = Array.make 64 0.0;
      np = 0.0; glnp = 0.0; plog = 0.0; pclog = 0.0; sq = 0.0;
      lfact = Array.make 1024 0.0 }
  in
  if nn <= 64 then (
    let pb = ref p in
    for i = 0 to 4 do
      pb := !pb *. 2.0;
      b.pbits.(i) <- 1 land int_of_float !pb
    done;
    { b with pb = !pb -. floor !pb; swch = 0 })
  else if float nn *. p < 30.0 then (
    b.cdf.(0) <- exp (float nn *. log (1.0 -. p));
    for i = 1 to 63 do
      b.cdf.(i) <-
        b.cdf.(i - 1)
        +. exp
             (gammln (float nn +. 1.0) -. gammln (float i +. 1.0)
             -. gammln (float (nn - i) +. 1.0)
             +. float i *. log p
             +. float (nn - i) *. log (1.0 -. p))
    done;
    { b with swch = 1 })
  else (
    let np = float nn *. p in
    for i = 0 to nn do
      if nn < 1024 then b.lfact.(i) <- gammln (float i +. 1.0)
    done;
    { b with np; glnp = gammln (float nn +. 1.0); plog = log p;
             pclog = log (1.0 -. p); sq = sqrt (np *. (1.0 -. p)); swch = 2 })

let binom_dev b =
  let k = ref 0 and cont = ref true in
  if b.swch = 0 then (
    let unfin = ref 0xFFFFFFFFFFFFFFFFL and rltp = ref 0L in
    for i = 0 to 4 do
      let sel = if b.pbits.(i) <> 0 then 0xFFFFFFFFFFFFFFFFL else 0L in
      let diff = Int64.logand !unfin (Int64.logxor (ran_step b.r) sel) in
      if b.pbits.(i) <> 0 then rltp := Int64.logor !rltp diff
      else rltp := Int64.logand !rltp (Int64.lognot diff);
      unfin := Int64.logand !unfin (Int64.lognot diff)
    done;
    k := 0;
    for _j = 0 to b.n - 1 do
      (if Int64.logand !unfin 1L <> 0L then (
         if ran_doub b.r < b.pb then k := !k + 1)
       else if Int64.logand !rltp 1L <> 0L then k := !k + 1);
      unfin := Int64.shift_right_logical !unfin 1;
      rltp := Int64.shift_right_logical !rltp 1
    done)
  else if b.swch = 1 then (
    let y = ran_doub b.r and kl = ref (-1) in
    k := 64;
    while !k - !kl > 1 do
      let km = (!kl + !k) / 2 in
      if y < b.cdf.(km) then k := km else kl := km
    done)
  else (
    while !cont do
      let u = 0.645 *. ran_doub b.r and v = -0.63 +. 1.25 *. ran_doub b.r in
      let v2 = v *. v in
      if
        (if v >= 0.0 then v2 > 6.5 *. u *. (0.645 -. u) *. (u +. 0.2)
         else v2 > 8.4 *. u *. (0.645 -. u) *. (u +. 0.1))
      then ()
      else (
        let kf = floor (b.sq *. (v /. u) +. b.np +. 0.5) in
        if kf >= 0.0 && kf <= float b.n then (
          k := int_of_float kf;
          let u2 = u *. u in
          if
            (if v >= 0.0 then v2 < 12.25 *. u2 *. (0.615 -. u) *. (0.92 -. u)
             else v2 < 7.84 *. u2 *. (0.615 -. u) *. (1.2 -. u))
          then cont := false
          else (
            let lf =
              if b.n < 1024 then b.lfact.(!k) +. b.lfact.(b.n - !k)
              else gammln (float !k +. 1.0) +. gammln (float (b.n - !k) +. 1.0)
            in
            let bb =
              b.sq *. exp (b.glnp +. float !k *. b.plog +. float (b.n - !k) *. b.pclog -. lf)
            in
            if u2 < bb then cont := false)))
    done);
  if b.p <> b.pp then b.n - !k else !k

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
  let f i = float_of_string toks.(i)
  and gu i = Int64.of_string toks.(i)
  and gi i = int_of_string toks.(i) in
  let op = toks.(0) in
  let out = ref [] in
  let emit_d v = out := fmt6 (canon v) :: !out in
  let emit_n n = out := string_of_int n :: !out in
  let nlast = gi (Array.length toks - 1) in
  (if op = "EXP" then (
     let r = ran_make (gu 2) and bet = f 1 in
     for _i = 1 to nlast do
       emit_d (expon_dev r bet)
     done)
   else if op = "LOGISTIC" then (
     let r = ran_make (gu 3) in
     for _i = 1 to nlast do
       emit_d (logistic_dev r (f 1) (f 2))
     done)
   else if op = "CAUCHY" then (
     let r = ran_make (gu 3) in
     for _i = 1 to nlast do
       emit_d (cauchy_dev r (f 1) (f 2))
     done)
   else if op = "NORMAL" then (
     let r = ran_make (gu 3) in
     for _i = 1 to nlast do
       emit_d (normal_dev r (f 1) (f 2))
     done)
   else if op = "BOXMULLER" then (
     let b = bm_make (f 1) (f 2) (gu 3) in
     for _i = 1 to nlast do
       emit_d (bm_dev b)
     done)
   else if op = "RAYLEIGH" then (
     let r = ran_make (gu 1) in
     for _i = 1 to nlast do
       let x = ref 0.0 and cont = ref true in
       while !cont do
         x := ran_doub r;
         if !x <> 0.0 then cont := false
       done;
       emit_d (sqrt (-2.0 *. log !x))
     done)
   else if op = "GAMMA" then (
     let g = gam_make (f 1) (f 2) (gu 3) in
     for _i = 1 to nlast do
       emit_d (gam_dev g)
     done)
   else if op = "CHISQ_DEV" then (
     let g = gam_make (0.5 *. f 1) 1.0 (gu 2) in
     for _i = 1 to nlast do
       emit_d (2.0 *. gam_dev g)
     done)
   else if op = "STUDENTT_DEV" then (
     let r = ran_make (gu 4) in
     for _i = 1 to nlast do
       let u1 = ref 0.0 and cont = ref true in
       while !cont do
         u1 := ran_doub r;
         if !u1 <> 0.0 then cont := false
       done;
       let u2 = ran_doub r in
       let y = sqrt ((!u1 ** (-2.0 /. f 1)) -. 1.0) *. cos (6.283185307179586 *. u2) in
       emit_d (f 2 +. f 3 *. y)
     done)
   else if op = "BETA_DEV" then (
     let dx = gam_make (f 1) 1.0 (gu 3) and dy = gam_make (f 2) 1.0 (Int64.add (gu 3) 1L) in
     for _i = 1 to nlast do
       let x = gam_dev dx and y = gam_dev dy in
       emit_d (x /. (x +. y))
     done)
   else if op = "F_DEV" then (
     let dx = gam_make (0.5 *. f 1) 1.0 (gu 3)
     and dy = gam_make (0.5 *. f 2) 1.0 (Int64.add (gu 3) 1L) in
     for _i = 1 to nlast do
       let x = gam_dev dx and y = gam_dev dy in
       let bb = x /. (x +. y) in
       emit_d (f 2 *. bb /. (f 1 *. (1.0 -. bb)))
     done)
   else if op = "POISSON" then (
     let d = { r = ran_make (gu 2); lam = f 1 } in
     for _i = 1 to nlast do
       emit_n (pois_dev d)
     done)
   else if op = "BINOMIAL" then (
     let b = binom_make (gi 1) (f 2) (gu 3) in
     for _i = 1 to nlast do
       emit_n (binom_dev b)
     done));
  print_endline (String.concat " " (List.rev !out))
