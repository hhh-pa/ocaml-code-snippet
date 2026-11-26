(* Statistical distributions (NR 6.14). *)

let fmt6 v =
  let w = if abs_float v < 5e-7 then 0.0 else v in
  Printf.sprintf "%.6f" (w +. 0.0)

let canon v = if abs_float v < 5e-7 then 0.0 else v

let geps = 2.220446049250313e-16
let gfpmin = 1.0020841800044864e-292

let gcof =
  [| 57.1562356658629235; -59.5979603554754912; 14.1360979747417471;
     -0.491913816097620199; 0.339946499848118887e-4; 0.465236289270485756e-4;
     -0.983744753048795646e-4; 0.158088703224912494e-3; -0.210264441724104883e-3;
     0.217439618115212643e-3; -0.164318106536763890e-3; 0.844182239838527433e-4;
     -0.261908384015814087e-4; 0.368991826595316234e-5 |]

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

let gammp_series a x gln =
  let ap = ref a and del = ref (1.0 /. a) and sum = ref (1.0 /. a) in
  let n = ref 0 in
  while !n < 1000 do
    ap := !ap +. 1.0;
    del := !del *. x /. !ap;
    sum := !sum +. !del;
    if abs_float !del < abs_float !sum *. geps then n := 1000 else n := !n + 1
  done;
  !sum *. exp (-.x +. a *. log x -. gln)

let gamma_cf a x gln upper =
  let b = ref (x +. 1.0 -. a) and c = ref (1.0 /. gfpmin) in
  let d = ref (1.0 /. (x +. 1.0 -. a)) and h = ref (1.0 /. (x +. 1.0 -. a)) in
  let i = ref 1 in
  while !i <= 1000 do
    let an = float (- !i) *. (float !i -. a) in
    b := !b +. 2.0;
    d := an *. !d +. !b;
    if abs_float !d < gfpmin then d := gfpmin;
    c := !b +. an /. !c;
    if abs_float !c < gfpmin then c := gfpmin;
    d := 1.0 /. !d;
    let del = !d *. !c in
    h := !h *. del;
    if abs_float (del -. 1.0) <= geps then i := 1001 else i := !i + 1
  done;
  let q = exp (-.x +. a *. log x -. gln) *. !h in
  if upper then q else 1.0 -. q

let gammp a x =
  if x = 0.0 then 0.0
  else (
    let gln = gammln a in
    if x < a +. 1.0 then gammp_series a x gln else gamma_cf a x gln false)

let gammq a x =
  if x = 0.0 then 1.0
  else (
    let gln = gammln a in
    if x < a +. 1.0 then 1.0 -. gammp_series a x gln else gamma_cf a x gln true)

let erfccheb z =
  let t = 2.0 /. (2.0 +. z) and ty = 4.0 *. (2.0 /. (2.0 +. z)) -. 2.0 in
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
    let x = ref (-0.70711 *. ((2.30753 +. t *. 0.27061) /. (1.0 +. t *. (0.99229 +. t *. 0.04481)) -. t)) in
    for _j = 0 to 1 do
      let err = erfcx !x -. pp in
      x := !x +. err /. (1.12837916709551257 *. exp (-. !x *. !x) -. !x *. err)
    done;
    if p < 1.0 then !x else -. !x)

let betacf a b x =
  let qab = a +. b and qap = a +. 1.0 and qam = a -. 1.0 in
  let c = ref 1.0 and d = ref (1.0 -. qab *. x /. qap) in
  if abs_float !d < gfpmin then d := gfpmin;
  d := 1.0 /. !d;
  let h = ref !d and m = ref 1 in
  while !m < 10000 do
    let m2 = 2 * !m in
    let aa =
      float !m *. (b -. float !m) *. x /. ((qam +. float m2) *. (a +. float m2))
    in
    d := 1.0 +. aa *. !d;
    if abs_float !d < gfpmin then d := gfpmin;
    c := 1.0 +. aa /. !c;
    if abs_float !c < gfpmin then c := gfpmin;
    d := 1.0 /. !d;
    h := !h *. !d *. !c;
    let aa =
      (-.a -. float !m) *. (qab +. float !m) *. x /. ((a +. float m2) *. (qap +. float m2))
    in
    d := 1.0 +. aa *. !d;
    if abs_float !d < gfpmin then d := gfpmin;
    c := 1.0 +. aa /. !c;
    if abs_float !c < gfpmin then c := gfpmin;
    d := 1.0 /. !d;
    let del = !d *. !c in
    h := !h *. del;
    if abs_float (del -. 1.0) <= geps then m := 10000 else m := !m + 1
  done;
  !h

let betai a b x =
  if x = 0.0 || x = 1.0 then x
  else (
    let bt =
      exp (gammln (a +. b) -. gammln a -. gammln b +. a *. log x +. b *. log (1.0 -. x))
    in
    if x < (a +. 1.0) /. (a +. b +. 2.0) then bt *. betacf a b x /. a
    else 1.0 -. bt *. betacf b a (1.0 -. x) /. b)

let invbetai p a b =
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
       let lna = log (a /. (a +. b)) and lnb = log (b /. (a +. b)) in
       let t = exp (a *. lna) /. a and u = exp (b *. lnb) /. b in
       let w = t +. u in
       if p < t /. w then x := (a *. w *. p) ** (1.0 /. a)
       else x := 1.0 -. (b *. w *. (1.0 -. p)) ** (1.0 /. b)));
    let afac = -.gammln a -. gammln b +. gammln (a +. b) in
    let j = ref 0 in
    while !j < 10 do
      if !x = 0.0 || !x = 1.0 then j := 10
      else (
        let err = betai a b !x -. p in
        let t = exp (a1 *. log !x +. b1 *. log (1.0 -. !x) +. afac) in
        let u = err /. t in
        let uu = ref (u *. (a1 /. !x -. b1 /. (1.0 -. !x))) in
        if !uu > 1.0 then uu := 1.0;
        let tt = u /. (1.0 -. 0.5 *. !uu) in
        x := !x -. tt;
        if !x <= 0.0 then x := 0.5 *. (!x +. tt);
        if !x >= 1.0 then x := 0.5 *. (!x +. tt +. 1.0);
        if abs_float tt < epsi *. !x && !j > 0 then j := 10 else j := !j + 1)
    done;
    !x)

let invgammp p a =
  let eps = 1.0e-8 in
  let gln = gammln a and a1 = a -. 1.0 in
  if p >= 1.0 then max 100.0 (a +. 100.0 *. sqrt a)
  else if p <= 0.0 then 0.0
  else (
    let x = ref 0.0 and lna1 = ref 0.0 and afac = ref 0.0 in
    (if a > 1.0 then (
       lna1 := log a1;
       afac := exp (a1 *. (!lna1 -. 1.0) -. gln);
       let pp = if p < 0.5 then p else 1.0 -. p in
       let t = sqrt (-2.0 *. log pp) in
       x := (2.30753 +. t *. 0.27061) /. (1.0 +. t *. (0.99229 +. t *. 0.04481)) -. t;
       if p < 0.5 then x := -. !x;
       x :=
         max 1.0e-3
           (a *. ((1.0 -. 1.0 /. (9.0 *. a) -. !x /. (3.0 *. sqrt a)) ** 3.0)))
     else (
       let t = 1.0 -. a *. (0.253 +. a *. 0.12) in
       if p < t then x := (p /. t) ** (1.0 /. a)
       else x := 1.0 -. log (1.0 -. (p -. t) /. (1.0 -. t))));
    let j = ref 0 in
    while !j < 12 do
      if !x <= 0.0 then j := 12
      else (
        let err = gammp a !x -. p in
        let t =
          if a > 1.0 then !afac *. exp (-.(!x -. a1) +. a1 *. (log !x -. !lna1))
          else exp (-. !x +. a1 *. log !x -. gln)
        in
        let u = err /. t in
        let uu = ref (u *. ((a -. 1.0) /. !x -. 1.0)) in
        if !uu > 1.0 then uu := 1.0;
        let tt = u /. (1.0 -. 0.5 *. !uu) in
        x := !x -. tt;
        if !x <= 0.0 then x := 0.5 *. (!x +. tt);
        if abs_float tt < eps *. !x then j := 12 else j := !j + 1)
    done;
    !x)

let invxlogx y =
  let ooe = 0.367879441171442322 in
  let u =
    ref
      (if y < -0.2 then log (ooe -. sqrt (2.0 *. ooe *. (y +. ooe))) else -10.0)
  and to_ = ref 0.0
  and cont = ref true in
  while !cont do
    let t = (log (y /. !u) -. !u) *. (!u /. (1.0 +. !u)) in
    u := !u +. t;
    if t < 1.0e-8 && abs_float (t +. !to_) < 0.01 *. abs_float t then cont := false
    else (
      to_ := t;
      if not (abs_float (t /. !u) > 1.0e-15) then cont := false)
  done;
  exp !u

let pks z =
  if z = 0.0 then 0.0
  else if z < 1.18 then (
    let y = exp (-1.23370055013616983 /. (z *. z)) in
    2.25675833419102515 *. sqrt (-.log y) *. (y +. y ** 9.0 +. y ** 25.0 +. y ** 49.0))
  else (
    let x = exp (-2.0 *. z *. z) in
    1.0 -. 2.0 *. (x -. x ** 4.0 +. x ** 9.0))

let qks z =
  if z = 0.0 then 1.0
  else if z < 1.18 then 1.0 -. pks z
  else (
    let x = exp (-2.0 *. z *. z) in
    2.0 *. (x -. x ** 4.0 +. x ** 9.0))

let invqks q =
  if q = 1.0 then 0.0
  else if q > 0.3 then (
    let t1 = 1.0 -. q in
    let f = -0.392699081698724155 *. t1 *. t1 in
    let y = ref (invxlogx f) and cont = ref true in
    while !cont do
      let logy = log !y in
      let s = 1.0 +. !y ** 4.0 +. !y ** 12.0 in
      let ff = f /. (s *. s) in
      let u = (!y *. logy -. ff) /. (1.0 +. logy) in
      let t = u /. max 0.5 (1.0 -. 0.5 *. u /. (!y *. (1.0 +. logy))) in
      y := !y -. t;
      if not (abs_float (t /. !y) > 1.0e-15) then cont := false
    done;
    1.57079632679489662 /. sqrt (-.log !y))
  else (
    let x = ref 0.03 and cont = ref true in
    while !cont do
      let xp = !x in
      x := 0.5 *. q +. !x ** 4.0 -. !x ** 9.0;
      if !x > 0.06 then x := !x +. !x ** 16.0 -. !x ** 25.0;
      if not (abs_float ((xp -. !x) /. !x) > 1.0e-15) then cont := false
    done;
    sqrt (-0.5 *. log !x))

let poisson_cdf lam n = if n = 0 then 0.0 else gammq (float n) lam

let poisson_inv lam p =
  if p < exp (-.lam) then 0
  else (
    let n = ref (int_of_float (max (sqrt lam) 5.0)) in
    let inc = ref 1 and nl = ref 0 and nu = ref 0 in
    (if p < poisson_cdf lam !n then (
       let cont = ref true in
       while !cont do
         n := (if !n - !inc > 0 then !n - !inc else 0);
         inc := !inc * 2;
         if not (p < poisson_cdf lam !n) then cont := false
       done;
       nl := !n;
       nu := !n + !inc / 2)
     else (
       let cont = ref true in
       while !cont do
         n := !n + !inc;
         inc := !inc * 2;
         if not (p > poisson_cdf lam !n) then cont := false
       done;
       nu := !n;
       nl := !n - !inc / 2));
    while !nu - !nl > 1 do
      n := (!nl + !nu) / 2;
      if p < poisson_cdf lam !n then nu := !n else nl := !n
    done;
    !nl)

let binomial_cdf n pe k =
  if k = 0 then 0.0 else if k > n then 1.0 else 1.0 -. betai (float k) (float (n - k + 1)) pe

let binomial_inv n pe p =
  let k = ref (int_of_float (float n *. pe)) in
  if !k < 0 then k := 0;
  if !k > n then k := n;
  let inc = ref 1 and kl = ref 0 and ku = ref 0 in
  (if p < binomial_cdf n pe !k then (
     let cont = ref true in
     while !cont do
       k := (if !k - !inc > 0 then !k - !inc else 0);
       inc := !inc * 2;
       if not (p < binomial_cdf n pe !k) then cont := false
     done;
     kl := !k;
     ku := !k + !inc / 2)
   else (
     let cont = ref true in
     while !cont do
       k := (if !k + !inc < n + 1 then !k + !inc else n + 1);
       inc := !inc * 2;
       if not (p > binomial_cdf n pe !k) then cont := false
     done;
     ku := !k;
     kl := !k - !inc / 2));
  while !ku - !kl > 1 do
    k := (!kl + !ku) / 2;
    if p < binomial_cdf n pe !k then ku := !k else kl := !k
  done;
  !kl

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
  let f i = float_of_string toks.(i) and li i = int_of_string toks.(i) in
  let op = toks.(0) in
  if op = "POISSON_INV" then Printf.printf "%d\n" (poisson_inv (f 1) (f 2))
  else if op = "BINOMIAL_INV" then
    Printf.printf "%d\n" (binomial_inv (li 1) (f 2) (f 3))
  else (
    let ans =
      if op = "NORMAL" then
        let mu, sg, x = (f 1, f 2, f 3) in
        0.5 *. erfcx (-0.707106781186547524 *. (x -. mu) /. sg)
      else if op = "NORMAL_INV" then
        let mu, sg, p = (f 1, f 2, f 3) in
        -1.41421356237309505 *. sg *. inverfc (2.0 *. p) +. mu
      else if op = "CAUCHY" then
        let mu, sg, x = (f 1, f 2, f 3) in
        0.5 +. 0.318309886183790671 *. atan2 (x -. mu) sg
      else if op = "CAUCHY_INV" then
        let mu, sg, p = (f 1, f 2, f 3) in
        mu +. sg *. tan (3.14159265358979324 *. (p -. 0.5))
      else if op = "STUDENTT" then
        let nu, mu, sg, t = (f 1, f 2, f 3, f 4) in
        let z = (t -. mu) /. sg in
        let p = 0.5 *. betai (0.5 *. nu) 0.5 (nu /. (nu +. z *. z)) in
        if t >= mu then 1.0 -. p else p
      else if op = "STUDENTT_INV" then
        let nu, mu, sg, p = (f 1, f 2, f 3, f 4) in
        let x = invbetai (2.0 *. min p (1.0 -. p)) (0.5 *. nu) 0.5 in
        let x = sg *. sqrt (nu *. (1.0 -. x) /. x) in
        if p >= 0.5 then mu +. x else mu -. x
      else if op = "STUDENTT_AA" then
        let nu, t = (f 1, f 2) in
        1.0 -. betai (0.5 *. nu) 0.5 (nu /. (nu +. t *. t))
      else if op = "STUDENTT_INVAA" then
        let nu, p = (f 1, f 2) in
        let x = invbetai (1.0 -. p) (0.5 *. nu) 0.5 in
        sqrt (nu *. (1.0 -. x) /. x)
      else if op = "LOGISTIC" then
        let mu, sg, x = (f 1, f 2, f 3) in
        let e = exp (-.abs_float (1.81379936423421785 *. (x -. mu) /. sg)) in
        if x >= mu then 1.0 /. (1.0 +. e) else e /. (1.0 +. e)
      else if op = "LOGISTIC_INV" then
        let mu, sg, p = (f 1, f 2, f 3) in
        mu +. 0.551328895421792049 *. sg *. log (p /. (1.0 -. p))
      else if op = "EXPON" then
        let bet, x = (f 1, f 2) in
        1.0 -. exp (-.bet *. x)
      else if op = "EXPON_INV" then
        let bet, p = (f 1, f 2) in
        -.log (1.0 -. p) /. bet
      else if op = "WEIBULL" then
        let alph, bet, x = (f 1, f 2, f 3) in
        1.0 -. exp (-.((x /. bet) ** alph))
      else if op = "WEIBULL_INV" then
        let alph, bet, p = (f 1, f 2, f 3) in
        bet *. (-.log (1.0 -. p)) ** (1.0 /. alph)
      else if op = "LOGNORMAL" then
        let mu, sg, x = (f 1, f 2, f 3) in
        if x = 0.0 then 0.0
        else 0.5 *. erfcx (-0.707106781186547524 *. (log x -. mu) /. sg)
      else if op = "LOGNORMAL_INV" then
        let mu, sg, p = (f 1, f 2, f 3) in
        exp (-1.41421356237309505 *. sg *. inverfc (2.0 *. p) +. mu)
      else if op = "CHISQ" then
        let nu, x = (f 1, f 2) in
        gammp (0.5 *. nu) (0.5 *. x)
      else if op = "CHISQ_INV" then
        let nu, p = (f 1, f 2) in
        2.0 *. invgammp p (0.5 *. nu)
      else if op = "GAMMA" then
        let alph, bet, x = (f 1, f 2, f 3) in
        gammp alph (bet *. x)
      else if op = "GAMMA_INV" then
        let alph, bet, p = (f 1, f 2, f 3) in
        invgammp p alph /. bet
      else if op = "FDIST" then
        let nu1, nu2, ff = (f 1, f 2, f 3) in
        betai (0.5 *. nu1) (0.5 *. nu2) (nu1 *. ff /. (nu2 +. nu1 *. ff))
      else if op = "FDIST_INV" then
        let nu1, nu2, p = (f 1, f 2, f 3) in
        let x = invbetai p (0.5 *. nu1) (0.5 *. nu2) in
        nu2 *. x /. (nu1 *. (1.0 -. x))
      else if op = "BETA" then
        let alph, bet, x = (f 1, f 2, f 3) in
        betai alph bet x
      else if op = "BETA_INV" then
        let alph, bet, p = (f 1, f 2, f 3) in
        invbetai p alph bet
      else if op = "KS_P" then pks (f 1)
      else if op = "KS_Q" then qks (f 1)
      else if op = "KS_INVQ" then invqks (f 1)
      else if op = "KS_INVP" then invqks (1.0 -. f 1)
      else if op = "POISSON" then poisson_cdf (f 1) (li 2)
      else binomial_cdf (li 1) (f 2) (li 3)
    in
    print_endline (fmt6 (canon ans)))
