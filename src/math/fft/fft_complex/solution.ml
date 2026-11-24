(* Radix-2 complex FFT (forward and inverse) by the Danielson-Lanczos
   lemma with bit-reversal reorder and trigonometric recurrence.
   Reference: Press et al., Numerical Recipes 3e, Section 12.2 (four1).
   Documented decisions: the book's truncated literal 6.28318530717959
   is used verbatim (not full-precision 2*pi); the inverse transform is
   NOT normalized (returns N times the inverse, as the book does --
   "you do that yourself"); n < 2 or not a power of 2 throws in the
   book, reported here as zero values with flag 1.
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
  if op = "FFT" || op = "IFFT" then (
    let isign = if op = "FFT" then 1 else -1 in
    let len = max 0 (2 * n) in
    let data = Array.init len (fun i -> float_of_string toks.(2 + i)) in
    try
      four1 data n isign;
      let parts = ref [] in
      for i = 0 to len - 1 do
        parts := fmt6 (canon data.(i)) :: !parts
      done;
      parts := "0" :: !parts;
      print_endline (String.concat " " (List.rev !parts))
    with BadN ->
      let parts = Array.to_list (Array.init len (fun _ -> "0.000000")) in
      let parts = parts @ ["1"] in
      print_endline (String.concat " " parts))
