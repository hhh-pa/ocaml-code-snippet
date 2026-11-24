(* Fast sine and cosine transforms via the real FFT plus
   auxiliary-array packing and odd/even unfolding.
   Reference: Press et al., Numerical Recipes 3e, Section 12.4
   (sinft/cosft1/cosft2, vendoring realft+four1 from 12.2-12.3).
   Documented decisions: sinft/cosft1 are self-inverse up to a
   factor (applying twice gives n/2 times the input -- "output
   should be multiplied by 2/n" for inverse use); cosft2 inverse
   likewise returns n/2 times the inverse; sizes must be powers
   of 2 >= 4 (realft needs n/2 >= 2), anything else reports zero
   values with flag 1; sinft forces y[0] = 0 (f_0 is identically
   0), ignoring input y[0]; the pi literal 3.141592653589793238
   is used verbatim.
   No online-judge problem found -- fixtures are self-generated
   (numpy oracle), no header link. *)

let pi = 3.141592653589793238

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
  let theta = pi /. float_of_int (n lsr 1) in
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

let sinft y =
  let n = Array.length y in
  if n < 4 || n land (n - 1) <> 0 then raise BadN;
  let theta = pi /. float_of_int n in
  let wtemp = sin (0.5 *. theta) in
  let wpr = (-2.0 *. wtemp) *. wtemp in
  let wpi = sin theta in
  let wr = ref 1.0 and wi = ref 0.0 in
  y.(0) <- 0.0;
  let j = ref 1 in
  while !j < (n lsr 1) + 1 do
    let wtemp = !wr in
    let q1 = wtemp *. wpr in
    let q2 = !wi *. wpi in
    wr := q1 -. q2 +. !wr;
    let q3 = !wi *. wpr in
    let q4 = wtemp *. wpi in
    wi := q3 +. q4 +. !wi;
    let y1 = !wi *. (y.(!j) +. y.(n - !j)) in
    let y2 = 0.5 *. (y.(!j) -. y.(n - !j)) in
    y.(!j) <- y1 +. y2;
    y.(n - !j) <- y1 -. y2;
    incr j
  done;
  realft y 1;
  y.(0) <- y.(0) *. 0.5;
  let sum = ref 0.0 in
  y.(1) <- 0.0;
  let j = ref 0 in
  while !j < n - 1 do
    sum := !sum +. y.(!j);
    y.(!j) <- y.(!j + 1);
    y.(!j + 1) <- !sum;
    j := !j + 2
  done

let cosft1 y =
  let n = Array.length y - 1 in
  if n < 4 || n land (n - 1) <> 0 then raise BadN;
  let theta = pi /. float_of_int n in
  let wtemp = sin (0.5 *. theta) in
  let wpr = (-2.0 *. wtemp) *. wtemp in
  let wpi = sin theta in
  let wr = ref 1.0 and wi = ref 0.0 in
  let yy = Array.make n 0.0 in
  let sum = ref (0.5 *. (y.(0) -. y.(n))) in
  yy.(0) <- 0.5 *. (y.(0) +. y.(n));
  let j = ref 1 in
  while !j < n / 2 do
    let wtemp = !wr in
    let q1 = wtemp *. wpr in
    let q2 = !wi *. wpi in
    wr := q1 -. q2 +. !wr;
    let q3 = !wi *. wpr in
    let q4 = wtemp *. wpi in
    wi := q3 +. q4 +. !wi;
    let y1 = 0.5 *. (y.(!j) +. y.(n - !j)) in
    let y2 = y.(!j) -. y.(n - !j) in
    let r1 = !wi *. y2 in
    yy.(!j) <- y1 -. r1;
    let r2 = !wi *. y2 in
    yy.(n - !j) <- y1 +. r2;
    let r3 = !wr *. y2 in
    sum := !sum +. r3;
    incr j
  done;
  yy.(n / 2) <- y.(n / 2);
  realft yy 1;
  for j = 0 to n - 1 do
    y.(j) <- yy.(j)
  done;
  y.(n) <- y.(1);
  y.(1) <- !sum;
  let j = ref 3 in
  while !j < n do
    sum := !sum +. y.(!j);
    y.(!j) <- !sum;
    j := !j + 2
  done

let cosft2 y isign =
  let n = Array.length y in
  if n < 4 || n land (n - 1) <> 0 then raise BadN;
  let theta = 0.5 *. pi /. float_of_int n in
  let wr1 = ref (cos theta) and wi1 = ref (sin theta) in
  let wpr = (-2.0 *. !wi1) *. !wi1 in
  let wpi = sin (2.0 *. theta) in
  let wr = ref 1.0 and wi = ref 0.0 in
  if isign = 1 then (
    let i = ref 0 in
    while !i < n / 2 do
      let y1 = 0.5 *. (y.(!i) +. y.(n - 1 - !i)) in
      let y2 = !wi1 *. (y.(!i) -. y.(n - 1 - !i)) in
      y.(!i) <- y1 +. y2;
      y.(n - 1 - !i) <- y1 -. y2;
      let wtemp = !wr1 in
      let q1 = wtemp *. wpr in
      let q2 = !wi1 *. wpi in
      wr1 := q1 -. q2 +. !wr1;
      let q3 = !wi1 *. wpr in
      let q4 = wtemp *. wpi in
      wi1 := q3 +. q4 +. !wi1;
      incr i
    done;
    realft y 1;
    let i = ref 2 in
    while !i < n do
      let wtemp = !wr in
      let q1 = wtemp *. wpr in
      let q2 = !wi *. wpi in
      wr := q1 -. q2 +. !wr;
      let q3 = !wi *. wpr in
      let q4 = wtemp *. wpi in
      wi := q3 +. q4 +. !wi;
      let s1 = y.(!i) *. !wr in
      let s2 = y.(!i + 1) *. !wi in
      let y1 = s1 -. s2 in
      let s3 = y.(!i + 1) *. !wr in
      let s4 = y.(!i) *. !wi in
      let y2 = s3 +. s4 in
      y.(!i) <- y1;
      y.(!i + 1) <- y2;
      i := !i + 2
    done;
    let sum = ref (0.5 *. y.(1)) in
    let i = ref (n - 1) in
    while !i > 0 do
      let sum1 = !sum in
      sum := !sum +. y.(!i);
      y.(!i) <- sum1;
      i := !i - 2
    done)
  else if isign = -1 then (
    let ytemp = y.(n - 1) in
    let i = ref (n - 1) in
    while !i > 2 do
      y.(!i) <- y.(!i - 2) -. y.(!i);
      i := !i - 2
    done;
    y.(1) <- 2.0 *. ytemp;
    let i = ref 2 in
    while !i < n do
      let wtemp = !wr in
      let q1 = wtemp *. wpr in
      let q2 = !wi *. wpi in
      wr := q1 -. q2 +. !wr;
      let q3 = !wi *. wpr in
      let q4 = wtemp *. wpi in
      wi := q3 +. q4 +. !wi;
      let s1 = y.(!i) *. !wr in
      let s2 = y.(!i + 1) *. !wi in
      let y1 = s1 +. s2 in
      let s3 = y.(!i + 1) *. !wr in
      let s4 = y.(!i) *. !wi in
      let y2 = s3 -. s4 in
      y.(!i) <- y1;
      y.(!i + 1) <- y2;
      i := !i + 2
    done;
    realft y (-1);
    let i = ref 0 in
    while !i < n / 2 do
      let y1 = y.(!i) +. y.(n - 1 - !i) in
      let y2 = (0.5 /. !wi1) *. (y.(!i) -. y.(n - 1 - !i)) in
      y.(!i) <- 0.5 *. (y1 +. y2);
      y.(n - 1 - !i) <- 0.5 *. (y1 -. y2);
      let wtemp = !wr1 in
      let q1 = wtemp *. wpr in
      let q2 = !wi1 *. wpi in
      wr1 := q1 -. q2 +. !wr1;
      let q3 = !wi1 *. wpr in
      let q4 = wtemp *. wpi in
      wi1 := q3 +. q4 +. !wi1;
      incr i
    done)

let emit y ok =
  let vals =
    if ok then Array.to_list (Array.map (fun v -> fmt6 (canon v)) y)
    else List.init (Array.length y) (fun _ -> "0.000000")
  in
  print_endline (String.concat " " (vals @ [if ok then "0" else "1"]))

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
  let n = int_of_string toks.(1) in
  if op = "SINFT" || op = "COSFT2F" || op = "COSFT2I" then (
    let len = max 0 n in
    let y = Array.init len (fun i -> float_of_string toks.(2 + i)) in
    let ok =
      try
        (if op = "SINFT" then sinft y
         else if op = "COSFT2F" then cosft2 y 1
         else cosft2 y (-1));
        true
      with BadN -> false
    in
    emit y ok)
  else if op = "COSFT1" then (
    let len = max 0 (n + 1) in
    let y = Array.init len (fun i -> float_of_string toks.(2 + i)) in
    let ok = try cosft1 y; true with BadN -> false in
    emit y ok)
