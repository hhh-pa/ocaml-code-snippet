(* Power spectrum estimation: windowed periodograms over segments
   (Spectreg), half-overlapping segments (Spectolap, incl. long-data
   accordion), and Slepian multitaper (filltable eigensolver).
   Reference: Press et al., Numerical Recipes 3e, Section 13.5
   (Spectreg/Spectolap/Slepian, vendoring realft+four1 from 12.2-12.3).
   Documented decisions: frequencies() is not output (closed form
   i*0.5/m); unknown window names flag; m must be a power of 2
   >= 1 (m = 1 flags via realft); SLEPIAN needs m >= 2 (its
   table build indexes u[3]); Newton breaks identically given
   identical libm-class inputs; SQR(x) is x*x.
   No online-judge problem found -- fixtures are self-generated
   (numpy/scipy oracle), no header link. *)

let fmt6 v =
  let w = if abs_float v < 5e-7 then 0.0 else v in
  Printf.sprintf "%.6f" (w +. 0.0)

let canon v = if abs_float v < 5e-7 then 0.0 else v

(* NOTE: float multiplies feeding +. or -. are let-bound first:
   ocamlopt fuses a direct mul-add/sub nest into FMA, which rounds
   once instead of twice and breaks bit-agreement with the C++
   reference (verified: zero FMA in the object code). *)

exception BadN

let four1 data n isign =
  if n < 2 || n land (n - 1) <> 0 then raise BadN;
  let nn = n lsl 1 in
  let j = ref 1 in
  let i = ref 1 in
  while !i < nn do
    (if !j > !i then (
       let t = data.(!j - 1) in
       data.(!j - 1) <- data.(!i - 1);
       data.(!i - 1) <- t;
       let t = data.(!j) in
       data.(!j) <- data.(!i);
       data.(!i) <- t));
    let m = ref n in
    while !m >= 2 && !j > !m do
      j := !j - !m;
      m := !m lsr 1
    done;
    j := !j + !m;
    i := !i + 2
  done;
  let mmax = ref 2 in
  while nn > !mmax do
    let istep = !mmax lsl 1 in
    let theta = float_of_int isign *. (6.28318530717959 /. float_of_int !mmax) in
    let wtemp = sin (0.5 *. theta) in
    let wpr = (-2.0 *. wtemp) *. wtemp in
    let wpi = sin theta in
    let wr = ref 1.0 and wi = ref 0.0 in
    let m = ref 1 in
    while !m < !mmax do
      let i = ref !m in
      while !i <= nn do
        let j = !i + !mmax in
        let p1 = !wr *. data.(j - 1) in
        let p2 = !wi *. data.(j) in
        let tempr = p1 -. p2 in
        let p3 = !wr *. data.(j) in
        let p4 = !wi *. data.(j - 1) in
        let tempi = p3 +. p4 in
        data.(j - 1) <- data.(!i - 1) -. tempr;
        data.(j) <- data.(!i) -. tempi;
        data.(!i - 1) <- data.(!i - 1) +. tempr;
        data.(!i) <- data.(!i) +. tempi;
        i := !i + istep
      done;
      let wtemp = !wr in
      let q1 = wtemp *. wpr in
      let q2 = !wi *. wpi in
      wr := q1 -. q2 +. !wr;
      let q3 = !wi *. wpr in
      let q4 = wtemp *. wpi in
      wi := q3 +. q4 +. !wi;
      m := !m + 2
    done;
    mmax := istep
  done

let realft data isign =
  let n = Array.length data in
  if n < 4 || n land (n - 1) <> 0 then raise BadN;
  let c1 = 0.5 in
  let theta = 3.141592653589793238 /. float_of_int (n lsr 1) in
  let c2, theta =
    if isign = 1 then (
      four1 data (n lsr 1) 1;
      (-0.5, theta))
    else (0.5, -.theta)
  in
  let wtemp = sin (0.5 *. theta) in
  let wpr = (-2.0 *. wtemp) *. wtemp in
  let wpi = sin theta in
  let wr = ref (1.0 +. wpr) and wi = ref wpi in
  let i = ref 1 in
  while !i < (n lsr 2) do
    let i1 = !i + !i in
    let i2 = 1 + i1 in
    let i3 = n - i1 in
    let i4 = 1 + i3 in
    let h1r = c1 *. (data.(i1) +. data.(i3)) in
    let h1i = c1 *. (data.(i2) -. data.(i4)) in
    let h2r = (-.c2) *. (data.(i2) +. data.(i4)) in
    let h2i = c2 *. (data.(i1) -. data.(i3)) in
    let p1 = !wr *. h2r in
    let p2 = !wi *. h2i in
    data.(i1) <- h1r +. p1 -. p2;
    let p3 = !wr *. h2i in
    let p4 = !wi *. h2r in
    data.(i2) <- h1i +. p3 +. p4;
    let p5 = !wr *. h2r in
    let p6 = !wi *. h2i in
    data.(i3) <- h1r -. p5 +. p6;
    let p7 = !wr *. h2i in
    let p8 = !wi *. h2r in
    data.(i4) <- (-.h1i) +. p7 +. p8;
    let wtemp = !wr in
    let q1 = wtemp *. wpr in
    let q2 = !wi *. wpi in
    wr := q1 -. q2 +. !wr;
    let q3 = !wi *. wpr in
    let q4 = wtemp *. wpi in
    wi := q3 +. q4 +. !wi;
    incr i
  done;
  if isign = 1 then (
    let h1r = data.(0) in
    data.(0) <- h1r +. data.(1);
    data.(1) <- h1r -. data.(1))
  else (
    let h1r = data.(0) in
    data.(0) <- c1 *. (h1r +. data.(1));
    data.(1) <- c1 *. (h1r -. data.(1));
    four1 data (n lsr 1) (-1))

type spectreg = {
  m : int;
  m2 : int;
  mutable nsum : int;
  specsum : float array;
  wksp : float array;
  mutable wtype : int;
  mutable hanntab : float array;
}

let make_spect m wt =
  if m < 1 || m land (m - 1) <> 0 then raise BadN;
  let s =
    { m; m2 = 2 * m; nsum = 0; specsum = Array.make (m + 1) 0.0;
      wksp = Array.make (2 * m) 0.0; wtype = wt; hanntab = [||] }
  in
  if wt = 3 then (
    let tab = Array.make (2 * m) 0.0 in
    let twopi = 8.0 *. atan 1.0 in
    for i = 0 to 2 * m - 1 do
      tab.(i) <- 0.5 *. (1.0 -. cos (float_of_int i *. twopi /. float_of_int (2 * m - 1)))
    done;
    s.hanntab <- tab);
  s

let window_val s j n =
  match s.wtype with
  | 1 -> 1.0 -. abs_float (2.0 *. float_of_int j /. float_of_int (n - 1) -. 1.0)
  | 2 ->
      let t = 2.0 *. float_of_int j /. float_of_int (n - 1) -. 1.0 in
      let p = t *. t in
      1.0 -. p
  | 3 ->
      if n <> s.m2 then raise BadN;
      s.hanntab.(j)
  | _ -> 1.0

let spect_addseg s data =
  if Array.length data <> s.m2 then raise BadN;
  let sumw = ref 0.0 in
  for i = 0 to s.m2 - 1 do
    let w = window_val s i s.m2 in
    s.wksp.(i) <- w *. data.(i);
    let p = w *. w in
    sumw := !sumw +. p
  done;
  let fac = 2.0 /. (!sumw *. float_of_int s.m2) in
  realft s.wksp 1;
  let q0 = s.wksp.(0) *. s.wksp.(0) in
  let r0 = 0.5 *. fac in
  let t0 = r0 *. q0 in
  s.specsum.(0) <- s.specsum.(0) +. t0;
  for i = 1 to s.m - 1 do
    let s1 = s.wksp.(2 * i) *. s.wksp.(2 * i) in
    let s2 = s.wksp.(2 * i + 1) *. s.wksp.(2 * i + 1) in
    let t = fac *. (s1 +. s2) in
    s.specsum.(i) <- s.specsum.(i) +. t
  done;
  let qm = s.wksp.(1) *. s.wksp.(1) in
  let tm = r0 *. qm in
  s.specsum.(s.m) <- s.specsum.(s.m) +. tm;
  s.nsum <- s.nsum + 1

let spect_spectrum s =
  if s.nsum = 0 then raise BadN;
  Array.init (s.m + 1) (fun i -> s.specsum.(i) /. float_of_int s.nsum)

type spectolap = {
  base : spectreg;
  mutable first : bool;
  fullseg : float array;
}

let make_olap m wt =
  let base = make_spect m wt in
  { base; first = true; fullseg = Array.make (2 * m) 0.0 }

let olap_addseg s data =
  let m = s.base.m in
  if Array.length data <> m then raise BadN;
  if s.first then (
    for i = 0 to m - 1 do
      s.fullseg.(i + m) <- data.(i)
    done;
    s.first <- false)
  else (
    for i = 0 to m - 1 do
      s.fullseg.(i) <- s.fullseg.(i + m);
      s.fullseg.(i + m) <- data.(i)
    done;
    spect_addseg s.base s.fullseg)

let olap_addlong s data =
  let m = s.base.m and m2 = s.base.m2 in
  let nt = Array.length data in
  let nk = (nt - 1) / m in
  let dl = if nk > 1 then float_of_int (nt - m2) /. float_of_int (nk - 1) else 0.0 in
  if nt < m2 then raise BadN;
  for k = 0 to nk - 1 do
    let kd = float_of_int k *. dl in
    let noff = int_of_float (kd +. 0.5) in
    for i = 0 to m2 - 1 do
      s.fullseg.(i) <- data.(noff + i)
    done;
    spect_addseg s.base s.fullseg
  done

type slepian = {
  base : spectreg;
  jres : int;
  kt : int;
  dpss : float array array;
  mutable p : float;
  mutable pp : float;
  mutable d : float;
  mutable dd : float;
}

let slep_renorm s n =
  s.p <- ldexp s.p n;
  s.pp <- ldexp s.pp n;
  s.d <- ldexp s.d n;
  s.dd <- ldexp s.dd n

let slep_filltable s =
  let m2 = s.base.m2 in
  let eps = 1.0e-10 and pi = 4.0 *. atan 1.0 in
  let jres = float_of_int s.jres in
  let dg = Array.make m2 0.0
  and dgg = Array.make m2 0.0
  and gam = Array.make m2 0.0
  and sup = Array.make (m2 - 1) 0.0
  and sub = Array.make (m2 - 1) 0.0 in
  let s0 = sin (jres *. pi /. float_of_int m2) in
  let sw = 2.0 *. s0 *. s0 in
  let t0 = float_of_int m2 -. 1.0 in
  let q0 = sw *. t0 *. t0 in
  dg.(0) <- 0.25 *. (float_of_int (2 * m2) +. q0 -. 1.0);
  for i = 1 to m2 - 1 do
    let ti = 2.0 *. float_of_int i in
    let t1 = float_of_int m2 -. 1.0 -. ti in
    let r1 = sw *. t1 *. t1 in
    let r2 = float_of_int (2 * (m2 - i) - 1) *. float_of_int (2 * i + 1) in
    dg.(i) <- 0.25 *. (r1 +. r2);
    sub.(i - 1) <- -.float_of_int i *. float_of_int (m2 - i) /. 2.0;
    sup.(i - 1) <- sub.(i - 1)
  done;
  let xx0a = 1.5692 *. jres in
  let xx = ref (-0.10859 -. 0.068762 /. jres +. xx0a)
  and xold = ref 0.0 in
  let xx0b = 3.1387 *. jres in
  xold := !xx +. 0.47276 +. 0.20273 /. jres -. xx0b;
  for k = 0 to s.kt - 1 do
    let u = s.dpss.(k) in
    let xnew = ref !xx in
    (try
       for _i = 0 to 19 do
         s.pp <- 1.0;
         s.p <- dg.(0) -. !xx;
         s.dd <- 0.0;
         s.d <- -1.0;
         for j = 1 to m2 - 1 do
           let ppp = s.pp in
           s.pp <- s.p;
           let ddd = s.dd in
           s.dd <- s.d;
           let s1 = sup.(j - 1) in
           let t1 = s.pp *. (dg.(j) -. !xx) in
           let t2 = ppp *. s1 *. s1 in
           s.p <- t1 -. t2;
           let u1 = s.dd *. (dg.(j) -. !xx) in
           let u2 = ddd *. s1 *. s1 in
           s.d <- -.s.pp +. u1 -. u2;
           if abs_float s.p > 1.0e30 then slep_renorm s (-100)
           else if abs_float s.p < 1.0e-30 then slep_renorm s 100
         done;
         xnew := !xx -. s.p /. s.d;
         if abs_float (!xx -. !xnew) < eps *. abs_float !xnew then raise Exit
         else xx := !xnew
       done
     with Exit -> ());
    xx := !xnew -. (!xold -. !xnew);
    xold := !xnew;
    for i = 0 to m2 - 1 do
      dgg.(i) <- dg.(i) -. !xnew
    done;
    let nl = m2 / 3 in
    dgg.(nl) <- 1.0;
    let ssup = sup.(nl) and ssub = sub.(nl - 1) in
    u.(0) <- 0.0;
    sup.(nl) <- 0.0;
    sub.(nl - 1) <- 0.0;
    let bet = ref dgg.(0) in
    for i = 1 to m2 - 1 do
      gam.(i) <- sup.(i - 1) /. !bet;
      let t = sub.(i - 1) *. gam.(i) in
      bet := dgg.(i) -. t;
      let vv = sub.(i - 1) *. u.(i - 1) in
      let v = (if i = nl then 1.0 else 0.0) -. vv in
      u.(i) <- v /. !bet
    done;
    for i = m2 - 2 downto 0 do
      let t = gam.(i + 1) *. u.(i + 1) in
      u.(i) <- u.(i) -. t
    done;
    sup.(nl) <- ssup;
    sub.(nl - 1) <- ssub;
    let sum = ref 0.0 in
    for i = 0 to m2 - 1 do
      let t = u.(i) *. u.(i) in
      sum := !sum +. t
    done;
    let sum = if u.(3) > 0.0 then sqrt !sum else -.sqrt !sum in
    for i = 0 to m2 - 1 do
      u.(i) <- u.(i) /. sum
    done
  done

let make_slepian m jres kt =
  if jres < 1 || kt >= 2 * jres then raise BadN;
  if m < 2 then raise BadN;
  let base = make_spect m (-1) in
  let s =
    { base; jres; kt; dpss = Array.init kt (fun _ -> Array.make (2 * m) 0.0);
      p = 0.0; pp = 0.0; d = 0.0; dd = 0.0 }
  in
  slep_filltable s;
  s

let slep_addseg s data =
  if Array.length data <> s.base.m2 then raise BadN;
  for k = 0 to s.kt - 1 do
    s.base.hanntab <- s.dpss.(k);
    s.base.wtype <- 3;
    spect_addseg s.base data
  done

let wtype_of = function
  | "SQUARE" -> 0
  | "BARTLETT" -> 1
  | "WELCH" -> 2
  | "HANN" -> 3
  | _ -> -1

let emit_zeros m =
  let w = if m >= 1 then m + 1 else 0 in
  let parts = List.init w (fun _ -> "0.000000") @ ["1"] in
  print_endline (String.concat " " parts)

let emit_spec spec =
  let vals = Array.to_list (Array.map (fun v -> fmt6 (canon v)) spec) in
  print_endline (String.concat " " (vals @ ["0"]))

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
  if op = "SLEPIAN" then (
    let m = int_of_string toks.(1)
    and jres = int_of_string toks.(2)
    and kt = int_of_string toks.(3)
    and k = int_of_string toks.(4) in
    let seglen = max 0 (2 * m) in
    let p = ref 5 in
    (try
       let s = make_slepian m jres kt in
       for _s = 0 to k - 1 do
         let seg =
           Array.init seglen (fun i ->
               let v = float_of_string toks.(!p + i) in
               v)
         in
         p := !p + seglen;
         slep_addseg s seg
       done;
       emit_spec (spect_spectrum s.base)
     with BadN -> emit_zeros m))
  else if op = "LONGDATA" then (
    let m = int_of_string toks.(1)
    and w = toks.(2)
    and nt = int_of_string toks.(3) in
    let nn = max 0 nt in
    let data = Array.init nn (fun i -> float_of_string toks.(4 + i)) in
    let wt = wtype_of w in
    (try
       if wt < 0 then raise BadN;
       let s = make_olap m wt in
       olap_addlong s data;
       emit_spec (spect_spectrum s.base)
     with BadN -> emit_zeros m))
  else if op = "PGRAM" || op = "OVERLAP" then (
    let m = int_of_string toks.(1)
    and w = toks.(2)
    and k = int_of_string toks.(3) in
    let seglen = max 0 (if op = "PGRAM" then 2 * m else m) in
    let wt = wtype_of w in
    let p = ref 4 in
    (try
       if wt < 0 then raise BadN;
       if op = "PGRAM" then (
         let s = make_spect m wt in
         for _s = 0 to k - 1 do
           let seg = Array.init seglen (fun i -> float_of_string toks.(!p + i)) in
           p := !p + seglen;
           spect_addseg s seg
         done;
         emit_spec (spect_spectrum s))
       else (
         let s = make_olap m wt in
         for _s = 0 to k - 1 do
           let seg = Array.init seglen (fun i -> float_of_string toks.(!p + i)) in
           p := !p + seglen;
           olap_addseg s seg
         done;
         emit_spec (spect_spectrum s.base))
     with BadN -> emit_zeros m))
