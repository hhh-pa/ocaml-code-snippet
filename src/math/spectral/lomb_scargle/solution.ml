(* Lomb-Scargle periodogram for unevenly sampled data: direct
   O(N^2) least-squares fit (period) and the FFT-based fast
   approximation with extirpolation (fasper/spread).
   Reference: Press et al., Numerical Recipes 3e, Section 13.9
   (period/fasper/spread, vendoring avevar from 14.2 and
   realft+four1 from 12.2-12.3).
   Documented decisions: n >= 2 enforced (n = 1 makes avevar's
   variance NaN, n = 0 overruns); nout must satisfy
   1 <= nout <= 131072 and fasper's nfreq <= 2^20 (robustness
   caps on ofac/hifac); period keeps the LAST frequency on ties
   (>=), fasper the FIRST (>), exactly as the book; abscissas
   must span a positive range (xdif = 0 is unguarded garbage
   in the book); exact-zero variance flags (the book throws).
   No online-judge problem found -- fixtures are self-generated
   (numpy oracle), no header link. *)

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

let avevar data =
  let n = Array.length data in
  if n < 2 then None
  else (
    let ave = ref 0.0 in
    for j = 0 to n - 1 do
      ave := !ave +. data.(j)
    done;
    ave := !ave /. float_of_int n;
    let vari = ref 0.0 and ep = ref 0.0 in
    for j = 0 to n - 1 do
      let s = data.(j) -. !ave in
      ep := !ep +. s;
      let q = s *. s in
      vari := !vari +. q
    done;
    let e2 = !ep *. !ep in
    vari := (!vari -. e2 /. float_of_int n) /. float_of_int (n - 1);
    Some (!ave, !vari))

let nfac = [| 0; 1; 1; 2; 6; 24; 120; 720; 5040; 40320; 362880 |]

let spread yv yy x m =
  if m > 10 then false
  else (
    let n = Array.length yy in
    let ix = int_of_float x in
    if x = float_of_int ix then yy.(ix - 1) <- yy.(ix - 1) +. yv
    else (
      let h = 0.5 *. float_of_int m in
      let t = x -. h in
      let ilo = ref (int_of_float t) in
      if !ilo < 0 then ilo := 0;
      if !ilo > n - m then ilo := n - m;
      let ihi = !ilo + m in
      let nden = ref nfac.(m) in
      let fac = ref (x -. float_of_int !ilo -. 1.0) in
      for j = !ilo + 1 to ihi - 1 do
        fac := !fac *. (x -. float_of_int j -. 1.0)
      done;
      yy.(ihi - 1) <-
        yy.(ihi - 1)
        +. yv *. !fac /. (float_of_int !nden *. (x -. float_of_int ihi));
      for j = ihi - 1 downto !ilo + 1 do
        nden := (!nden / (j - !ilo)) * (j - ihi);
        yy.(j - 1) <-
          yy.(j - 1)
          +. yv *. !fac /. (float_of_int !nden *. (x -. float_of_int j))
      done);
    true)

let twopi = 6.283185307179586476

let lomb_prob pmax nout ofac =
  let expy = exp (-.pmax) in
  let effm = 2.0 *. float_of_int nout /. ofac in
  let prob = effm *. expy in
  if prob > 0.01 then 1.0 -. (1.0 -. expy) ** effm else prob

type lomb_res = { px : float array; py : float array; jmax : int; prob : float }

let period x y ofac hifac =
  let n = Array.length x in
  if n < 2 || ofac <= 0.0 || hifac <= 0.0 then None
  else (
    let noutd = 0.5 *. ofac *. hifac *. float_of_int n in
    if not (noutd >= 1.0 && noutd <= 131072.0) then None
    else (
      let nout = int_of_float noutd in
      match avevar y with
      | None -> None
      | Some (ave, vari) ->
        if vari = 0.0 then None
        else (
          let xmin = ref x.(0) and xmax = ref x.(0) in
          for j = 1 to n - 1 do
            if x.(j) < !xmin then xmin := x.(j);
            if x.(j) > !xmax then xmax := x.(j)
          done;
          let xdif = !xmax -. !xmin in
          if not (xdif > 0.0) then None
          else (
            let wr = Array.make n 0.0 in
            let wi = Array.make n 0.0 in
            let wpr = Array.make n 0.0 in
            let wpi = Array.make n 0.0 in
            let px = Array.make nout 0.0 in
            let py = Array.make nout 0.0 in
            let xave = 0.5 *. (!xmin +. !xmax) in
            let p0 = 1.0 /. (xdif *. ofac) in
            for j = 0 to n - 1 do
              let dx = x.(j) -. xave in
              let inner = dx *. p0 in
              let wpi0 = twopi *. inner in
              let s = sin (0.5 *. wpi0) in
              wpr.(j) <- (-2.0 *. s) *. s;
              wpi.(j) <- sin wpi0;
              wr.(j) <- cos wpi0;
              wi.(j) <- sin wpi0
            done;
            let jmax = ref 0 and pymax = ref 0.0 in
            for i = 0 to nout - 1 do
              let pnow = float_of_int (i + 1) /. (ofac *. xdif) in
              px.(i) <- pnow;
              let sumsh = ref 0.0 and sumc = ref 0.0 in
              for j = 0 to n - 1 do
                let a = pnow *. twopi in
                let b = x.(j) -. xave in
                let c = cos (a *. b) in
                let s = sin (a *. b) in
                let q1 = s *. c in
                sumsh := !sumsh +. q1;
                let q2 = (c -. s) *. (c +. s) in
                sumc := !sumc +. q2
              done;
              let wtau = 0.5 *. atan2 (2.0 *. !sumsh) !sumc in
              let swtau = sin wtau and cwtau = cos wtau in
              let sums = ref 0.0 in
              sumc := 0.0;
              let sumsy = ref 0.0 and sumcy = ref 0.0 in
              for j = 0 to n - 1 do
                let q1 = wr.(j) *. swtau in
                let q2 = cwtau *. wi.(j) in
                let ss = q1 -. q2 in
                let q3 = wr.(j) *. cwtau in
                let q4 = swtau *. wi.(j) in
                let cc = q3 +. q4 in
                let r1 = ss *. ss in
                sums := !sums +. r1;
                let r2 = cc *. cc in
                sumc := !sumc +. r2;
                let yy = y.(j) -. ave in
                let r3 = yy *. ss in
                sumsy := !sumsy +. r3;
                let r4 = yy *. cc in
                sumcy := !sumcy +. r4;
                let wtemp = wr.(j) in
                let a1 = wtemp *. wpr.(j) in
                let a2 = wi.(j) *. wpi.(j) in
                wr.(j) <- a1 -. a2 +. wr.(j);
                let b1 = wi.(j) *. wpr.(j) in
                let b2 = wtemp *. wpi.(j) in
                wi.(j) <- b1 +. b2 +. wi.(j)
              done;
              let t1 = !sumcy *. !sumcy in
              let t2 = !sumsy *. !sumsy in
              py.(i) <- 0.5 *. (t1 /. !sumc +. t2 /. !sums) /. vari;
              if py.(i) >= !pymax then (
                pymax := py.(i);
                jmax := i)
            done;
            Some { px; py; jmax = !jmax; prob = lomb_prob !pymax nout ofac }))))

let fasper x y ofac hifac =
  let macc = 4 in
  let n = Array.length x in
  if n < 2 || ofac <= 0.0 || hifac <= 0.0 then None
  else (
    let noutd = 0.5 *. ofac *. hifac *. float_of_int n in
    if not (noutd >= 1.0 && noutd <= 131072.0) then None
    else (
      let nout = int_of_float noutd in
      let nfreqt = ofac *. hifac *. float_of_int n *. float_of_int macc in
      let nfreq = ref 64 in
      let bad = ref false in
      while float_of_int !nfreq < nfreqt && not !bad do
        nfreq := !nfreq lsl 1;
        if !nfreq > 1048576 then bad := true
      done;
      if !bad then None
      else (
        let nwk = !nfreq lsl 1 in
        let wk1 = Array.make nwk 0.0 in
        let wk2 = Array.make nwk 0.0 in
        match avevar y with
        | None -> None
        | Some (ave, vari) ->
          if vari = 0.0 then None
          else (
            let xmin = ref x.(0) and xmax = ref x.(0) in
            for j = 1 to n - 1 do
              if x.(j) < !xmin then xmin := x.(j);
              if x.(j) > !xmax then xmax := x.(j)
            done;
            let xdif = !xmax -. !xmin in
            if not (xdif > 0.0) then None
            else (
              let fac = float_of_int nwk /. (xdif *. ofac) in
              let fndim = float_of_int nwk in
              let df = 1.0 /. (xdif *. ofac) in
              let pmax = ref (-1.0) and jmax = ref 0 in
              let px = Array.make nout 0.0 in
              let py = Array.make nout 0.0 in
              for j = 0 to n - 1 do
                let ck = mod_float ((x.(j) -. !xmin) *. fac) fndim in
                let ckk = 2.0 *. ck in
                let ck = ck +. 1.0 in
                let ckk = mod_float ckk fndim +. 1.0 in
                ignore (spread (y.(j) -. ave) wk1 ck macc);
                ignore (spread 1.0 wk2 ckk macc)
              done;
              if not (realft wk1 1) then None
              else if not (realft wk2 1) then None
              else (
                let k = ref 2 in
                for j = 0 to nout - 1 do
                  let s1 = wk2.(!k) *. wk2.(!k) in
                  let s2 = wk2.(!k + 1) *. wk2.(!k + 1) in
                  let hypo = sqrt (s1 +. s2) in
                  let hc2wt = 0.5 *. wk2.(!k) /. hypo in
                  let hs2wt = 0.5 *. wk2.(!k + 1) /. hypo in
                  let cwt = sqrt (0.5 +. hc2wt) in
                  let s = sqrt (0.5 -. hc2wt) in
                  let swt = if hs2wt < 0.0 then -.s else s in
                  let q0 = 0.5 *. float_of_int n in
                  let q1 = hc2wt *. wk2.(!k) in
                  let q2 = hs2wt *. wk2.(!k + 1) in
                  let den = q0 +. q1 +. q2 in
                  let r1 = cwt *. wk1.(!k) in
                  let r2 = swt *. wk1.(!k + 1) in
                  let t1 = r1 +. r2 in
                  let cterm = t1 *. t1 /. den in
                  let r3 = cwt *. wk1.(!k + 1) in
                  let r4 = swt *. wk1.(!k) in
                  let t2 = r3 -. r4 in
                  let sterm = t2 *. t2 /. (float_of_int n -. den) in
                  px.(j) <- float_of_int (j + 1) *. df;
                  py.(j) <- (cterm +. sterm) /. (2.0 *. vari);
                  if py.(j) > !pmax then (
                    pmax := py.(j);
                    jmax := j);
                  k := !k + 2
                done;
                Some { px; py; jmax = !jmax; prob = lomb_prob !pmax nout ofac }))))))

let fail_nout n ofac hifac =
  if n >= 2 && ofac > 0.0 && hifac > 0.0 then (
    let nd = 0.5 *. ofac *. hifac *. float_of_int n in
    if nd >= 1.0 then (if nd <= 131072.0 then int_of_float nd else 131072) else 0)
  else 0

let emit_lomb r =
  let parts = ref [] in
  Array.iter (fun v -> parts := fmt6 (canon v) :: !parts) r.px;
  Array.iter (fun v -> parts := fmt6 (canon v) :: !parts) r.py;
  parts := "0" :: fmt6 (canon r.prob) :: string_of_int r.jmax :: !parts;
  print_endline (String.concat " " (List.rev !parts))

let emit_fail nout =
  let zeros = List.init (2 * nout) (fun _ -> "0.000000") in
  print_endline (String.concat " " (zeros @ [ "0"; "0.000000"; "1" ]))

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
  if op = "LOMB" || op = "FASTLOMB" then (
    let n = int_of_string toks.(1) in
    let nn = max 0 n in
    let x = Array.init nn (fun i -> float_of_string toks.(2 + i)) in
    let y = Array.init nn (fun i -> float_of_string toks.(2 + nn + i)) in
    let ofac = float_of_string toks.(2 + 2 * nn) in
    let hifac = float_of_string toks.(3 + 2 * nn) in
    let r = if op = "LOMB" then period x y ofac hifac else fasper x y ofac hifac in
    match r with
    | Some r -> emit_lomb r
    | None -> emit_fail (fail_nout n ofac hifac))
