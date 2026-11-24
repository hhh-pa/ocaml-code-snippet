(* Fourier integrals I = Int_a^b exp(iwt)h(t) dt via FFT with cubic
   endpoint corrections (dftcor) and the illustrative driver (dftint)
   with polynomial interpolation of the DFT spectrum.
   Reference: Press et al., Numerical Recipes 3e, Section 13.10
   (dftcor/dftint, vendoring Poly_interp::rawinterp from 3.2 and
   realft+four1 from 12.2-12.3).
   Documented decisions: M = 64, NDFT = 1024, MPOL = 6 are fixed
   exactly as printed (the book calls them merely illustrative,
   but the printed program is what is extracted); h(t) arrives as
   M + 1 explicit samples, not a callback (memcof-style explicit
   input precedent); Poly_interp(xpol, ., MPOL).interp(en) is exactly
   rawinterp(0, en) because mm == n clamps locate/hunt to jlo = 0
   (fresh object, so cor = 0 takes the locate path); dftcor's throw
   (a >= b, th < 0, th > 3.1416) and rawinterp's den == 0 become
   value flags; divisions stay divisions (t2/45.0 etc. are NOT
   rewritten as multiplies).
   No online-judge problem found -- fixtures are self-generated
   (scipy/numpy oracle), no header link. *)

let fmt6 v =
  let w = if abs_float v < 5e-7 then 0.0 else v in
  Printf.sprintf "%.6f" (w +. 0.0)

let canon v = if abs_float v < 5e-7 then 0.0 else v

(* NOTE: float multiplies feeding +. or -. are let-bound first:
   ocamlopt fuses a direct mul-add/sub nest into FMA, which rounds
   once instead of twice and breaks bit-agreement with the C++
   reference (verified: zero FMA in the object code). *)

let four1 data n isign =
  if n < 2 || (n land (n - 1)) <> 0 then false
  else (
    let nn = n lsl 1 in
    let j = ref 1 in
    let i = ref 1 in
    while !i < nn do
      if !j > !i then (
        let t = data.(!j - 1) in
        data.(!j - 1) <- data.(!i - 1);
        data.(!i - 1) <- t;
        let t = data.(!j) in
        data.(!j) <- data.(!i);
        data.(!i) <- t);
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
          let jj = !i + !mmax in
          let q1 = !wr *. data.(jj - 1) in
          let q2 = !wi *. data.(jj) in
          let tempr = q1 -. q2 in
          let q3 = !wr *. data.(jj) in
          let q4 = !wi *. data.(jj - 1) in
          let tempi = q3 +. q4 in
          data.(jj - 1) <- data.(!i - 1) -. tempr;
          data.(jj) <- data.(!i) -. tempi;
          data.(!i - 1) <- data.(!i - 1) +. tempr;
          data.(!i) <- data.(!i) +. tempi;
          i := !i + istep
        done;
        let wtemp = !wr in
        let r1 = wtemp *. wpr in
        let r2 = !wi *. wpi in
        wr := r1 -. r2 +. !wr;
        let r3 = !wi *. wpr in
        let r4 = wtemp *. wpi in
        wi := r3 +. r4 +. !wi;
        m := !m + 2
      done;
      mmax := istep
    done;
    true)

let realft data isign =
  let n = Array.length data in
  if n < 4 || (n land (n - 1)) <> 0 then false
  else (
    let c1 = 0.5 in
    let half = n lsr 1 in
    let c2 = if isign = 1 then -0.5 else 0.5 in
    let theta =
      (if isign = 1 then 1.0 else -1.0)
      *. (3.141592653589793238 /. float_of_int half)
    in
    if isign = 1 then ignore (four1 data half 1);
    let wtemp = sin (0.5 *. theta) in
    let wpr = (-2.0 *. wtemp) *. wtemp in
    let wpi = sin theta in
    let wr = ref (1.0 +. wpr) and wi = ref wpi in
    for i = 1 to (n lsr 2) - 1 do
      let i1 = i + i in
      let i2 = 1 + i1 in
      let i3 = n - i1 in
      let i4 = 1 + i3 in
      let h1r = c1 *. (data.(i1) +. data.(i3)) in
      let h1i = c1 *. (data.(i2) -. data.(i4)) in
      let h2r = (-.c2) *. (data.(i2) +. data.(i4)) in
      let h2i = c2 *. (data.(i1) -. data.(i3)) in
      let a1 = !wr *. h2r in
      let a2 = !wi *. h2i in
      data.(i1) <- h1r +. a1 -. a2;
      let b1 = !wr *. h2i in
      let b2 = !wi *. h2r in
      data.(i2) <- h1i +. b1 +. b2;
      let c_1 = !wr *. h2r in
      let c_2 = !wi *. h2i in
      data.(i3) <- h1r -. c_1 +. c_2;
      let d1 = !wr *. h2i in
      let d2 = !wi *. h2r in
      data.(i4) <- (-.h1i) +. d1 +. d2;
      let wtemp = !wr in
      let r1 = wtemp *. wpr in
      let r2 = !wi *. wpi in
      wr := r1 -. r2 +. !wr;
      let r3 = !wi *. wpr in
      let r4 = wtemp *. wpi in
      wi := r3 +. r4 +. !wi
    done;
    if isign = 1 then (
      let h1r = data.(0) in
      data.(0) <- h1r +. data.(1);
      data.(1) <- h1r -. data.(1))
    else (
      let h1r = data.(0) in
      data.(0) <- c1 *. (h1r +. data.(1));
      data.(1) <- c1 *. (h1r -. data.(1));
      ignore (four1 data half (-1)));
    true)

let poly_raw xa ya x =
  let mm = 6 in
  let c = Array.make mm 0.0 in
  let d = Array.make mm 0.0 in
  let ns = ref 0 in
  let dif = ref (abs_float (x -. xa.(0))) in
  for i = 0 to mm - 1 do
    let dift = abs_float (x -. xa.(i)) in
    if dift < !dif then (
      ns := i;
      dif := dift);
    c.(i) <- ya.(i);
    d.(i) <- ya.(i)
  done;
  let y = ref ya.(!ns) in
  ns := !ns - 1;
  let bad = ref false in
  let m = ref 1 in
  while !m < mm && not !bad do
    let i = ref 0 in
    while !i < mm - !m && not !bad do
      let ho = xa.(!i) -. x in
      let hp = xa.(!i + !m) -. x in
      let w = c.(!i + 1) -. d.(!i) in
      let den0 = ho -. hp in
      if den0 = 0.0 then bad := true
      else (
        let den = w /. den0 in
        d.(!i) <- hp *. den;
        c.(!i) <- ho *. den);
      i := !i + 1
    done;
    if not !bad then (
      if 2 * (!ns + 1) < mm - !m then y := !y +. c.(!ns + 1)
      else (
        y := !y +. d.(!ns);
        ns := !ns - 1);
      m := !m + 1)
  done;
  if !bad then None else Some !y

let dftcor w delta a b endpts =
  let th = w *. delta in
  if a >= b || th < 0.0 || th > 3.1416 then None
  else (
    let a0r = ref 0.0 in
    let a1r = ref 0.0 in
    let a2r = ref 0.0 in
    let a3r = ref 0.0 in
    let a0i = ref 0.0 in
    let a1i = ref 0.0 in
    let a2i = ref 0.0 in
    let a3i = ref 0.0 in
    let corfac = ref 0.0 in
    (if abs_float th < 5.0e-2 then (
       let t = th in
       let t2 = t *. t in
       let t4 = t2 *. t2 in
       let t6 = t4 *. t2 in
       let k1 = 11.0 /. 720.0 in
       let k2 = 23.0 /. 15120.0 in
       let m1 = k1 *. t4 in
       let m2 = k2 *. t6 in
       corfac := 1.0 -. m1 +. m2;
       let c0 = -2.0 /. 3.0 in
       let d1 = t2 /. 45.0 in
       let k3 = 103.0 /. 15120.0 in
       let m3 = k3 *. t4 in
       let k4 = 169.0 /. 226800.0 in
       let m4 = k4 *. t6 in
       a0r := c0 +. d1 +. m3 -. m4;
       let c1 = 7.0 /. 24.0 in
       let k5 = 7.0 /. 180.0 in
       let m5 = k5 *. t2 in
       let k6 = 5.0 /. 3456.0 in
       let m6 = k6 *. t4 in
       let k7 = 7.0 /. 259200.0 in
       let m7 = k7 *. t6 in
       a1r := c1 -. m5 +. m6 -. m7;
       let c2 = -1.0 /. 6.0 in
       let k8 = 5.0 /. 6048.0 in
       let m8 = k8 *. t4 in
       let d2 = t6 /. 64800.0 in
       a2r := c2 +. d1 -. m8 +. d2;
       let c3 = 1.0 /. 24.0 in
       let d3 = t2 /. 180.0 in
       let k9 = 5.0 /. 24192.0 in
       let m9 = k9 *. t4 in
       let d4 = t6 /. 259200.0 in
       a3r := c3 -. d3 +. m9 -. d4;
       let e0 = 2.0 /. 45.0 in
       let k10 = 2.0 /. 105.0 in
       let n1 = k10 *. t2 in
       let k11 = 8.0 /. 2835.0 in
       let n2 = k11 *. t4 in
       let k12 = 86.0 /. 467775.0 in
       let n3 = k12 *. t6 in
       a0i := t *. (e0 +. n1 -. n2 +. n3);
       let e1 = 7.0 /. 72.0 in
       let d5 = t2 /. 168.0 in
       let k13 = 11.0 /. 72576.0 in
       let n4 = k13 *. t4 in
       let k14 = 13.0 /. 5987520.0 in
       let n5 = k14 *. t6 in
       a1i := t *. (e1 -. d5 +. n4 -. n5);
       let e2 = -7.0 /. 90.0 in
       let d6 = t2 /. 210.0 in
       let k15 = 11.0 /. 90720.0 in
       let n6 = k15 *. t4 in
       let k16 = 13.0 /. 7484400.0 in
       let n7 = k16 *. t6 in
       a2i := t *. (e2 +. d6 -. n6 +. n7);
       let e3 = 7.0 /. 360.0 in
       let d7 = t2 /. 840.0 in
       let k17 = 11.0 /. 362880.0 in
       let n8 = k17 *. t4 in
       let k18 = 13.0 /. 29937600.0 in
       let n9 = k18 *. t6 in
       a3i := t *. (e3 -. d7 +. n8 -. n9))
     else (
       let cth = cos th in
       let sth = sin th in
       let p1 = cth *. cth in
       let p2 = sth *. sth in
       let ctth = p1 -. p2 in
       let stth = (2.0 *. sth) *. cth in
       let th2 = th *. th in
       let th4 = th2 *. th2 in
       let tmth2 = 3.0 -. th2 in
       let spth2 = 6.0 +. th2 in
       let sth4i = 1.0 /. ((6.0 *. th4)) in
       let tth4i = 2.0 *. sth4i in
       let q0 = 4.0 *. cth in
       let inner0 = 3.0 -. q0 +. ctth in
       let outer0 = tth4i *. spth2 in
       corfac := outer0 *. inner0;
       let q1 = 5.0 *. th2 in
       let q2 = 8.0 *. cth in
       let inner1 = q2 -. ctth in
       let q3 = spth2 *. inner1 in
       a0r := sth4i *. ((-42.0) +. q1 +. q3);
       let q4 = 6.0 *. th2 in
       let inner2 = (-12.0) +. q4 in
       let q5 = th *. inner2 in
       let q6 = spth2 *. stth in
       a0i := sth4i *. (q5 +. q6);
       let q7 = 14.0 *. tmth2 in
       let i1 = 7.0 *. spth2 in
       let q8 = i1 *. cth in
       a1r := sth4i *. (q7 -. q8);
       let q9 = 30.0 *. th in
       let i2 = 5.0 *. spth2 in
       let q10 = i2 *. sth in
       a1i := sth4i *. (q9 -. q10);
       let q11 = (-4.0) *. tmth2 in
       let i3 = 2.0 *. spth2 in
       let q12 = i3 *. cth in
       a2r := tth4i *. (q11 +. q12);
       let q13 = (-12.0) *. th in
       let q14 = i3 *. sth in
       a2i := tth4i *. (q13 +. q14);
       let q15 = 2.0 *. tmth2 in
       let q16 = spth2 *. cth in
       a3r := sth4i *. (q15 -. q16);
       let q17 = 6.0 *. th in
       let q18 = spth2 *. sth in
       a3i := sth4i *. (q17 -. q18)));
    let l1 = !a0r *. endpts.(0) in
    let l2 = !a1r *. endpts.(1) in
    let l3 = !a2r *. endpts.(2) in
    let l4 = !a3r *. endpts.(3) in
    let cl = l1 +. l2 +. l3 +. l4 in
    let s1 = !a0i *. endpts.(0) in
    let s2 = !a1i *. endpts.(1) in
    let s3 = !a2i *. endpts.(2) in
    let s4 = !a3i *. endpts.(3) in
    let sl = s1 +. s2 +. s3 +. s4 in
    let r1 = !a0r *. endpts.(7) in
    let r2 = !a1r *. endpts.(6) in
    let r3 = !a2r *. endpts.(5) in
    let r4 = !a3r *. endpts.(4) in
    let cr = r1 +. r2 +. r3 +. r4 in
    let na0i = -.(!a0i) in
    let t1 = na0i *. endpts.(7) in
    let t2 = !a1i *. endpts.(6) in
    let t3 = !a2i *. endpts.(5) in
    let t4 = !a3i *. endpts.(4) in
    let sr = t1 -. t2 -. t3 -. t4 in
    let arg = w *. (b -. a) in
    let c = cos arg in
    let s = sin arg in
    let u1 = c *. cr in
    let u2 = s *. sr in
    let corre = cl +. u1 -. u2 in
    let v1 = s *. cr in
    let v2 = c *. sr in
    let corim = sl +. v1 +. v2 in
    Some (corre, corim, !corfac))

let dftint a b w h =
  let m = 64 and ndft = 1024 and mpol = 6 in
  let twopi = 6.283185307179586476 in
  if Array.length h <> m + 1 then None
  else (
    let delta = (b -. a) /. float_of_int m in
    let data = Array.make ndft 0.0 in
    for j = 0 to m do
      data.(j) <- h.(j)
    done;
    let endpts = Array.make 8 0.0 in
    for j = 0 to 3 do
      endpts.(j) <- data.(j);
      endpts.(j + 4) <- data.(m - 3 + j)
    done;
    if not (realft data 1) then None
    else (
      data.(1) <- 0.0;
      let en =
        (w *. delta *. float_of_int ndft /. twopi) +. 1.0
      in
      let h3 = 0.5 *. float_of_int mpol in
      let nn0 = int_of_float (en -. h3 +. 1.0) in
      let nnmax = ndft / 2 - mpol + 1 in
      let nn = ref (min (max nn0 1) nnmax) in
      let cpol = Array.make mpol 0.0 in
      let spol = Array.make mpol 0.0 in
      let xpol = Array.make mpol 0.0 in
      for j = 0 to mpol - 1 do
        cpol.(j) <- data.(2 * !nn - 2);
        spol.(j) <- data.(2 * !nn - 1);
        xpol.(j) <- float_of_int !nn;
        nn := !nn + 1
      done;
      (match (poly_raw xpol cpol en, poly_raw xpol spol en) with
       | Some cdft, Some sdft -> (
         match dftcor w delta a b endpts with
         | None -> None
         | Some (corre, corim, corfac) ->
           let p1 = cdft *. corfac in
           let cdft = p1 +. corre in
           let p2 = sdft *. corfac in
           let sdft = p2 +. corim in
           let c = delta *. cos (w *. a) in
           let s = delta *. sin (w *. a) in
           let o1 = c *. cdft in
           let o2 = s *. sdft in
           let cosint = o1 -. o2 in
           let o3 = s *. cdft in
           let o4 = c *. sdft in
           let sinint = o3 +. o4 in
           Some (cosint, sinint))
       | _ -> None)))

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
  if op = "DFTCOR" then (
    let w = float_of_string toks.(1) in
    let delta = float_of_string toks.(2) in
    let a = float_of_string toks.(3) in
    let b = float_of_string toks.(4) in
    let endpts = Array.init 8 (fun i -> float_of_string toks.(5 + i)) in
    (match dftcor w delta a b endpts with
     | Some (corre, corim, corfac) ->
       print_endline
         (fmt6 (canon corre)
         ^ " "
         ^ fmt6 (canon corim)
         ^ " "
         ^ fmt6 (canon corfac)
         ^ " 0")
     | None -> print_endline "0.000000 0.000000 0.000000 1"))
  else if op = "DFTINT" then (
    let a = float_of_string toks.(1) in
    let b = float_of_string toks.(2) in
    let w = float_of_string toks.(3) in
    let h = Array.init 65 (fun i -> float_of_string toks.(4 + i)) in
    (match dftint a b w h with
     | Some (cosint, sinint) ->
       print_endline (fmt6 (canon cosint) ^ " " ^ fmt6 (canon sinint) ^ " 0")
     | None -> print_endline "0.000000 0.000000 1"))
