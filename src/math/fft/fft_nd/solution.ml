(* N-dimensional complex FFT (forward and inverse) by successive
   one-dimensional Danielson-Lanczos passes with per-dimension
   bit-reversal.
   Reference: Press et al., Numerical Recipes 3e, Section 12.5 (fourn).
   Documented decisions: validity is tested on the product of the
   lengths only (ntot >= 2 a power of 2), exactly as the book does --
   for positive dims this is equivalent to testing each dim, and
   length-1 dims are faithful no-ops; the inverse is NOT normalized
   (returns ntot times the inverse); theta uses the book's integer
   division ifp2/ip1 and left-assoc isign*6.28.../k; the truncated
   literal 6.28318530717959 is used verbatim. Robustness: ntot is
   capped at 10^6 (beyond that, or non-positive dims, the flag-1
   path runs with output width clamped to 2M values).
   No online-judge problem found -- fixtures are self-generated
   (numpy oracle), no header link. *)

let lim = 1000000

let fmt6 v =
  let w = if abs_float v < 5e-7 then 0.0 else v in
  Printf.sprintf "%.6f" (w +. 0.0)

let canon v = if abs_float v < 5e-7 then 0.0 else v

(* NOTE: float multiplies feeding +. or -. are let-bound first:
   ocamlopt fuses a direct mul-add/sub nest into FMA, which rounds
   once instead of twice and breaks bit-agreement with the C++
   reference (verified: zero FMA in the object code). *)

exception BadN

let fourn data nn isign =
  let ndim = Array.length nn in
  let ntot = ref 1 in
  Array.iter (fun d -> ntot := !ntot * d) nn;
  if !ntot < 2 || !ntot land (!ntot - 1) <> 0 then raise BadN;
  let nprev = ref 1 in
  let idim = ref (ndim - 1) in
  while !idim >= 0 do
    let n = nn.(!idim) in
    let nrem = !ntot / (n * !nprev) in
    let ip1 = !nprev lsl 1 in
    let ip2 = ip1 * n in
    let ip3 = ip2 * nrem in
    let i2rev = ref 0 in
    let i2 = ref 0 in
    while !i2 < ip2 do
      (if !i2 < !i2rev then (
         let i1 = ref !i2 in
         while !i1 < !i2 + ip1 - 1 do
           let i3 = ref !i1 in
           while !i3 < ip3 do
             let i3rev = !i2rev + !i3 - !i2 in
             let t = data.(!i3) in
             data.(!i3) <- data.(i3rev);
             data.(i3rev) <- t;
             let t = data.(!i3 + 1) in
             data.(!i3 + 1) <- data.(i3rev + 1);
             data.(i3rev + 1) <- t;
             i3 := !i3 + ip2
           done;
           i1 := !i1 + 2
         done));
      let ibit = ref (ip2 lsr 1) in
      while !ibit >= ip1 && !i2rev + 1 > !ibit do
        i2rev := !i2rev - !ibit;
        ibit := !ibit lsr 1
      done;
      i2rev := !i2rev + !ibit;
      i2 := !i2 + ip1
    done;
    let ifp1 = ref ip1 in
    while !ifp1 < ip2 do
      let ifp2 = !ifp1 lsl 1 in
      let theta =
        float_of_int isign *. 6.28318530717959 /. float_of_int (ifp2 / ip1)
      in
      let wtemp = sin (0.5 *. theta) in
      let wpr = (-2.0 *. wtemp) *. wtemp in
      let wpi = sin theta in
      let wr = ref 1.0 and wi = ref 0.0 in
      let i3 = ref 0 in
      while !i3 < !ifp1 do
        let i1 = ref !i3 in
        while !i1 < !i3 + ip1 - 1 do
          let i2 = ref !i1 in
          while !i2 < ip3 do
            let k1 = !i2 in
            let k2 = k1 + !ifp1 in
            let p1 = !wr *. data.(k2) in
            let p2 = !wi *. data.(k2 + 1) in
            let tempr = p1 -. p2 in
            let p3 = !wr *. data.(k2 + 1) in
            let p4 = !wi *. data.(k2) in
            let tempi = p3 +. p4 in
            data.(k2) <- data.(k1) -. tempr;
            data.(k2 + 1) <- data.(k1 + 1) -. tempi;
            data.(k1) <- data.(k1) +. tempr;
            data.(k1 + 1) <- data.(k1 + 1) +. tempi;
            i2 := !i2 + ifp2
          done;
          i1 := !i1 + 2
        done;
        let wtemp = !wr in
        let q1 = wtemp *. wpr in
        let q2 = !wi *. wpi in
        wr := q1 -. q2 +. !wr;
        let q3 = !wi *. wpr in
        let q4 = wtemp *. wpi in
        wi := q3 +. q4 +. !wi;
        i3 := !i3 + ip1
      done;
      ifp1 := ifp2
    done;
    nprev := !nprev * n;
    decr idim
  done

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
  let ndim = int_of_string toks.(1) in
  let ndim_us = max 0 ndim in
  let nn = Array.init ndim_us (fun i -> int_of_string toks.(2 + i)) in
  let ntot = ref 1 in
  let stop = ref false in
  Array.iter
    (fun d ->
      if not !stop then (
        if d <= 0 || !ntot > lim || d > lim then (
          ntot := (if d <= 0 then 0 else lim + 1);
          stop := true)
        else (
          ntot := !ntot * d;
          if !ntot > lim then (
            ntot := lim + 1;
            stop := true))))
    nn;
  if ndim <= 0 then ntot := 1;
  let valid = !ntot >= 2 && !ntot <= lim && !ntot land (!ntot - 1) = 0 in
  let nvals =
    if valid then 2 * !ntot
    else if !ntot <= 0 then 0
    else min (2 * !ntot) (2 * lim)
  in
  if op = "FFTND" || op = "IFFTND" then (
    let isign = if op = "FFTND" then 1 else -1 in
    let base = 2 + ndim_us in
    let data = Array.init nvals (fun i -> float_of_string toks.(base + i)) in
    let ok = valid && (try fourn data nn isign; true with BadN -> false) in
    let vals =
      if ok then Array.to_list (Array.map (fun v -> fmt6 (canon v)) data)
      else List.init nvals (fun _ -> "0.000000")
    in
    print_endline (String.concat " " (vals @ [if ok then "0" else "1"])))
