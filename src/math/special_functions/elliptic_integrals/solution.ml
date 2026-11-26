(* Elliptic integrals and Jacobian elliptic functions (NR 6.12).
   Including the documented sncndn /= to *= correction. *)

let fmt6 v =
  let w = if abs_float v < 5e-7 then 0.0 else v in
  Printf.sprintf "%.6f" (w +. 0.0)

let canon v = if abs_float v < 5e-7 then 0.0 else v

let fmin3 a b c = if a < b then (if a < c then a else c) else if b < c then b else c
let fmax2 a b = if a > b then a else b
let fmax3 a b c = fmax2 (fmax2 a b) c

let ell_rc x y =
  let errtol = 0.0012 and third = 1.0 /. 3.0 and c1 = 0.3 and c2 = 1.0 /. 7.0
  and c3 = 0.375 and c4 = 9.0 /. 22.0 in
  let xt = ref (if y > 0.0 then x else x -. y)
  and yt = ref (if y > 0.0 then y else -.y) in
  let w = if y > 0.0 then 1.0 else sqrt x /. sqrt !xt in
  let s = ref 0.0 and ave = ref 0.0 and cont = ref true in
  while !cont do
    let sx = sqrt !xt in
    let sy = sqrt !yt in
    let p1 = 2.0 *. sx in
    let p2 = p1 *. sy in
    let al = p2 +. !yt in
    xt := 0.25 *. (!xt +. al);
    yt := 0.25 *. (!yt +. al);
    ave := third *. (!xt +. !yt +. !yt);
    s := (!yt -. !ave) /. !ave;
    if not (abs_float !s > errtol) then cont := false
  done;
  let i4 = !s *. c4 in
  let i3 = c3 +. i4 in
  let m3 = !s *. i3 in
  let i2 = c2 +. m3 in
  let m2 = !s *. i2 in
  let i1 = c1 +. m2 in
  let s2 = !s *. !s in
  let m1 = s2 *. i1 in
  let inner = 1.0 +. m1 in
  let num = w *. inner in
  num /. sqrt !ave

let ell_rd x y z =
  let errtol = 0.0015 and c1 = 3.0 /. 14.0 and c2 = 1.0 /. 6.0
  and c3 = 9.0 /. 22.0 and c4 = 3.0 /. 26.0 and c5 = 0.25 *. (9.0 /. 22.0)
  and c6 = 1.5 *. (3.0 /. 26.0) in
  let xt = ref x and yt = ref y and zt = ref z in
  let sm = ref 0.0 and fac = ref 1.0 in
  let delx = ref 0.0 and dely = ref 0.0 and delz = ref 0.0 and ave = ref 0.0 in
  let cont = ref true in
  while !cont do
    let sx = sqrt !xt and sy = sqrt !yt and sz = sqrt !zt in
    let syz = sy +. sz in
    let p1 = sx *. syz in
    let p2 = sy *. sz in
    let al = p1 +. p2 in
    sm := !sm +. !fac /. (sz *. (!zt +. al));
    fac := 0.25 *. !fac;
    xt := 0.25 *. (!xt +. al);
    yt := 0.25 *. (!yt +. al);
    zt := 0.25 *. (!zt +. al);
    let t3 = 3.0 *. !zt in
    ave := 0.2 *. (!xt +. !yt +. t3);
    delx := (!ave -. !xt) /. !ave;
    dely := (!ave -. !yt) /. !ave;
    delz := (!ave -. !zt) /. !ave;
    if not (fmax3 (abs_float !delx) (abs_float !dely) (abs_float !delz) > errtol) then
      cont := false
  done;
  let ea = !delx *. !dely in
  let eb = !delz *. !delz in
  let ec = ea -. eb in
  let t6 = 6.0 *. eb in
  let ed = ea -. t6 in
  let ee = ed +. ec +. ec in
  let t3s = 3.0 *. !sm in
  let k1 = c5 *. ed in
  let k2 = c6 *. !delz in
  let k3 = k2 *. ee in
  let xv = (-.c1 +. k1) -. k3 in
  let m1 = ed *. xv in
  let i1 = 1.0 +. m1 in
  let l1 = c2 *. ee in
  let n1 = (-.c3) *. ec in
  let n2 = !delz *. c4 in
  let n3 = n2 *. ea in
  let inner2 = n1 +. n3 in
  let m2 = !delz *. inner2 in
  let yv = l1 +. m2 in
  let m3 = !delz *. yv in
  let inner = i1 +. m3 in
  let num = !fac *. inner in
  let den = !ave *. sqrt !ave in
  t3s +. num /. den

let rec ell_rj x y z p =
  let errtol = 0.0015 and c1 = 3.0 /. 14.0 and c2 = 1.0 /. 3.0
  and c3 = 3.0 /. 22.0 and c4 = 3.0 /. 26.0 and c5 = 0.75 *. (3.0 /. 22.0)
  and c6 = 1.5 *. (3.0 /. 26.0) and c7 = 0.5 *. (1.0 /. 3.0)
  and c8 = (3.0 /. 22.0) +. (3.0 /. 22.0) in
  let xt = ref 0.0 and yt = ref 0.0 and zt = ref 0.0 and pt = ref 0.0 in
  let a = ref 0.0 and b = ref 0.0 and rcx = ref 0.0 in
  if p > 0.0 then (
    xt := x;
    yt := y;
    zt := z;
    pt := p)
  else (
    xt := fmin3 x y z;
    zt := fmax3 x y z;
    yt := x +. y +. z -. !xt -. !zt;
    a := 1.0 /. (!yt -. p);
    b := !a *. (!zt -. !yt) *. (!yt -. !xt);
    pt := !yt +. !b;
    let rho = !xt *. !zt /. !yt and tau = p *. !pt /. !yt in
    rcx := ell_rc rho tau);
  let sm = ref 0.0 and fac = ref 1.0 in
  let delx = ref 0.0 and dely = ref 0.0 and delz = ref 0.0 and delp = ref 0.0
  and ave = ref 0.0 in
  let cont = ref true in
  while !cont do
    let sx = sqrt !xt and sy = sqrt !yt and sz = sqrt !zt in
    let syz = sy +. sz in
    let p1 = sx *. syz in
    let p2 = sy *. sz in
    let al = p1 +. p2 in
    let s3 = sx +. sy +. sz in
    let q1 = !pt *. s3 in
    let r1 = sx *. sy in
    let r2 = r1 *. sz in
    let t1 = q1 +. r2 in
    let alpha = t1 *. t1 in
    let t2 = !pt +. al in
    let beta = !pt *. t2 *. t2 in
    let rc0 = ell_rc alpha beta in
    let p3 = !fac *. rc0 in
    sm := !sm +. p3;
    fac := 0.25 *. !fac;
    xt := 0.25 *. (!xt +. al);
    yt := 0.25 *. (!yt +. al);
    zt := 0.25 *. (!zt +. al);
    pt := 0.25 *. (!pt +. al);
    ave := 0.2 *. (!xt +. !yt +. !zt +. !pt +. !pt);
    delx := (!ave -. !xt) /. !ave;
    dely := (!ave -. !yt) /. !ave;
    delz := (!ave -. !zt) /. !ave;
    delp := (!ave -. !pt) /. !ave;
    let m =
      fmax2 (fmax2 (abs_float !delx) (abs_float !dely))
        (fmax2 (abs_float !delz) (abs_float !delp))
    in
    if not (m > errtol) then cont := false
  done;
  let s1 = !dely +. !delz in
  let p1 = !delx *. s1 in
  let p2 = !dely *. !delz in
  let ea = p1 +. p2 in
  let q1 = !delx *. !dely in
  let eb = q1 *. !delz in
  let ec = !delp *. !delp in
  let t3 = 3.0 *. ec in
  let ed = ea -. t3 in
  let r1 = 2.0 *. !delp in
  let sube = ea -. ec in
  let r2 = r1 *. sube in
  let ee = eb +. r2 in
  let t3s = 3.0 *. !sm in
  let k1 = c5 *. ed in
  let k2 = c6 *. ee in
  let xv = (-.c1 +. k1) -. k2 in
  let m1 = ed *. xv in
  let i1 = 1.0 +. m1 in
  let l1 = !delp *. c4 in
  let y1 = (-.c8) +. l1 in
  let m2 = !delp *. y1 in
  let yv = c7 +. m2 in
  let m3 = eb *. yv in
  let i2 = i1 +. m3 in
  let n1 = !delp *. ea in
  let z1 = !delp *. c3 in
  let zv = c2 -. z1 in
  let m4 = n1 *. zv in
  let i3 = i2 +. m4 in
  let o1 = c2 *. !delp in
  let o2 = o1 *. ec in
  let inner = i3 -. o2 in
  let num = !fac *. inner in
  let den = !ave *. sqrt !ave in
  let ans = t3s +. num /. den in
  if p <= 0.0 then (
    let rf0 = ell_rf !xt !yt !zt in
    let sub = !rcx -. rf0 in
    let m5 = 3.0 *. sub in
    let m6 = !b *. ans in
    let inner2 = m6 +. m5 in
    !a *. inner2)
  else ans

and ell_rf x y z =
  let errtol = 0.0025 and third = 1.0 /. 3.0 and c1 = 1.0 /. 24.0 and c2 = 0.1
  and c3 = 3.0 /. 44.0 and c4 = 1.0 /. 14.0 in
  let xt = ref x and yt = ref y and zt = ref z in
  let delx = ref 0.0 and dely = ref 0.0 and delz = ref 0.0 and ave = ref 0.0 in
  let cont = ref true in
  while !cont do
    let sx = sqrt !xt and sy = sqrt !yt and sz = sqrt !zt in
    let syz = sy +. sz in
    let p1 = sx *. syz in
    let p2 = sy *. sz in
    let al = p1 +. p2 in
    xt := 0.25 *. (!xt +. al);
    yt := 0.25 *. (!yt +. al);
    zt := 0.25 *. (!zt +. al);
    ave := third *. (!xt +. !yt +. !zt);
    delx := (!ave -. !xt) /. !ave;
    dely := (!ave -. !yt) /. !ave;
    delz := (!ave -. !zt) /. !ave;
    if not (fmax3 (abs_float !delx) (abs_float !dely) (abs_float !delz) > errtol) then
      cont := false
  done;
  let t1 = !delx *. !dely in
  let t2 = !delz *. !delz in
  let e2 = t1 -. t2 in
  let e3 = t1 *. !delz in
  let k1 = c1 *. e2 in
  let k2 = c3 *. e3 in
  let inner = (k1 -. c2) -. k2 in
  let m1 = inner *. e2 in
  let m2 = c4 *. e3 in
  let num = 1.0 +. m1 +. m2 in
  num /. sqrt !ave

let ell_f phi ak =
  let s = sin phi and c = cos phi in
  let cc = c *. c in
  let m1 = s *. ak in
  let m2 = s *. ak in
  let q = (1.0 -. m1) *. (1.0 +. m2) in
  let r = ell_rf cc q 1.0 in
  s *. r

let ell_e phi ak =
  let s = sin phi and c = cos phi in
  let cc = c *. c in
  let m1 = s *. ak in
  let m2 = s *. ak in
  let q = (1.0 -. m1) *. (1.0 +. m2) in
  let t = s *. ak in
  let rf0 = ell_rf cc q 1.0 in
  let rd0 = ell_rd cc q 1.0 in
  let t2 = t *. t in
  let m3 = t2 *. rd0 in
  let d3 = m3 /. 3.0 in
  let sub = rf0 -. d3 in
  s *. sub

let ell_pi phi en ak =
  let s = sin phi and c = cos phi in
  let e1 = en *. s in
  let enss = e1 *. s in
  let cc = c *. c in
  let m1 = s *. ak in
  let m2 = s *. ak in
  let q = (1.0 -. m1) *. (1.0 +. m2) in
  let rf0 = ell_rf cc q 1.0 in
  let rj0 = ell_rj cc q 1.0 (1.0 +. enss) in
  let m3 = enss *. rj0 in
  let d3 = m3 /. 3.0 in
  let sub = rf0 -. d3 in
  s *. sub

let ell_sn uu emmc =
  let ca = 1.0e-8 in
  if emmc = 0.0 then (
    let cn = 1.0 /. cosh uu in
    (tanh uu, cn, cn))
  else (
    let bo = emmc < 0.0 in
    let d = ref 0.0 and emc = ref emmc and u = ref uu in
    if bo then (
      d := 1.0 -. emmc;
      emc := !emc *. (-1.0 /. !d);
      d := sqrt !d;
      u := !u *. !d);
    let a = ref 1.0 and dn = ref 1.0 and c = ref 0.0 in
    let em = Array.make 13 0.0 and en = Array.make 13 0.0 in
    let l = ref 0 and i = ref 0 in
    while !i < 13 do
      l := !i;
      em.(!i) <- !a;
      emc := sqrt !emc;
      en.(!i) <- !emc;
      c := 0.5 *. (!a +. !emc);
      if abs_float (!a -. !emc) <= ca *. !a then i := 13
      else (
        emc := !emc *. !a;
        a := !c;
        i := !i + 1)
    done;
    u := !u *. !c;
    let sn = ref (sin !u) and cn = ref (cos !u) in
    if !sn <> 0.0 then (
      a := !cn /. !sn;
      c := !c *. !a;
      let ii = ref !l in
      while !ii >= 0 do
        let b = em.(!ii) in
        a := !a *. !c;
        c := !c *. !dn;
        dn := (en.(!ii) +. !a) /. (b +. !a);
        a := !c /. b;
        ii := !ii - 1
      done;
      let p = !c *. !c in
      a := 1.0 /. sqrt (p +. 1.0);
      sn := (if !sn >= 0.0 then !a else -. !a);
      cn := !c *. !sn);
    if bo then (
      a := !dn;
      dn := !cn;
      cn := !a;
      sn := !sn /. !d);
    (!sn, !cn, !dn))

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
  let f i = float_of_string toks.(i) in
  let op = toks.(0) in
  if op = "SN" then (
    let (sn, cn, dn) = ell_sn (f 1) (f 2) in
    Printf.printf "%s %s %s\n" (fmt6 (canon sn)) (fmt6 (canon cn)) (fmt6 (canon dn)))
  else (
    let ans =
      if op = "RF" then ell_rf (f 1) (f 2) (f 3)
      else if op = "RD" then ell_rd (f 1) (f 2) (f 3)
      else if op = "RJ" then ell_rj (f 1) (f 2) (f 3) (f 4)
      else if op = "RC" then ell_rc (f 1) (f 2)
      else if op = "ELLF" then ell_f (f 1) (f 2)
      else if op = "ELLE" then ell_e (f 1) (f 2)
      else ell_pi (f 1) (f 2) (f 3)
    in
    print_endline (fmt6 (canon ans)))
