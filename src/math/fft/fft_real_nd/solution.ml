(* FFT of real data in two and three dimensions by packing into a
   half-length complex FFT plus split/recombine with a Nyquist
   side array.
   Reference: Press et al., Numerical Recipes 3e, Section 12.6 (rlft3,
   vendoring the fourn engine from 12.5).
   Documented decisions: the inverse returns (n1*n2*n3/2) times the
   inverse; dims must be powers of 2 with nn1,nn2 >= 1 and nn3 >= 2
   (nn3 = 1 would index out of bounds in the book's inverse path),
   plus fourn's own ntot >= 2 check -- anything else reports zero
   values with flag 1; theta uses isign*(6.28.../nn3) as written;
   the truncated literal 6.28318530717959 is used verbatim.
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

let ispow2 n = n >= 1 && n land (n - 1) = 0

let rlft3 data speq isign nn1 nn2 nn3 =
  if not (ispow2 nn1) || not (ispow2 nn2) || nn3 < 2 || nn3 land (nn3 - 1) <> 0
  then raise BadN;
  let spq a b = speq.(a * 2 * nn2 + b) in
  let setspq a b v = speq.(a * 2 * nn2 + b) <- v in
  let c1 = 0.5 in
  let c2 = -0.5 *. float_of_int isign in
  let theta = float_of_int isign *. (6.28318530717959 /. float_of_int nn3) in
  let wtemp = sin (0.5 *. theta) in
  let wpr = (-2.0 *. wtemp) *. wtemp in
  let wpi = sin theta in
  let nn = [|nn1; nn2; nn3 lsr 1|] in
  (if isign = 1 then (
     fourn data nn isign;
     let k1 = ref 0 in
     for i1 = 0 to nn1 - 1 do
       let j2 = ref 0 in
       for _i2 = 0 to nn2 - 1 do
         setspq i1 !j2 data.(!k1);
         incr j2;
         setspq i1 !j2 data.(!k1 + 1);
         incr j2;
         k1 := !k1 + nn3
       done
     done));
  for i1 = 0 to nn1 - 1 do
    let j1 = if i1 <> 0 then nn1 - i1 else 0 in
    let wr = ref 1.0 and wi = ref 0.0 in
    let i3 = ref 0 in
    while !i3 <= (nn3 lsr 1) do
      let k1 = ref (i1 * nn2 * nn3) in
      let k3 = j1 * nn2 * nn3 in
      for i2 = 0 to nn2 - 1 do
        (if !i3 = 0 then (
           let j2 = if i2 <> 0 then (nn2 - i2) lsl 1 else 0 in
           let h1r = c1 *. (data.(!k1) +. spq j1 j2) in
           let h1i = c1 *. (data.(!k1 + 1) -. spq j1 (j2 + 1)) in
           let h2i = c2 *. (data.(!k1) -. spq j1 j2) in
           let h2r = (-.c2) *. (data.(!k1 + 1) +. spq j1 (j2 + 1)) in
           data.(!k1) <- h1r +. h2r;
           data.(!k1 + 1) <- h1i +. h2i;
           setspq j1 j2 (h1r -. h2r);
           setspq j1 (j2 + 1) (h2i -. h1i))
         else (
           let j2 = if i2 <> 0 then nn2 - i2 else 0 in
           let j3 = nn3 - !i3 in
           let k2 = !k1 + !i3 in
           let k4 = k3 + j2 * nn3 + j3 in
           let h1r = c1 *. (data.(k2) +. data.(k4)) in
           let h1i = c1 *. (data.(k2 + 1) -. data.(k4 + 1)) in
           let h2i = c2 *. (data.(k2) -. data.(k4)) in
           let h2r = (-.c2) *. (data.(k2 + 1) +. data.(k4 + 1)) in
           let p1 = !wr *. h2r in
           let p2 = !wi *. h2i in
           data.(k2) <- h1r +. p1 -. p2;
           let p3 = !wr *. h2i in
           let p4 = !wi *. h2r in
           data.(k2 + 1) <- h1i +. p3 +. p4;
           let p5 = !wr *. h2r in
           let p6 = !wi *. h2i in
           data.(k4) <- h1r -. p5 +. p6;
           let p7 = !wr *. h2i in
           let p8 = !wi *. h2r in
           data.(k4 + 1) <- (-.h1i) +. p7 +. p8));
        k1 := !k1 + nn3
      done;
      let wtemp = !wr in
      let q1 = wtemp *. wpr in
      let q2 = !wi *. wpi in
      wr := q1 -. q2 +. !wr;
      let q3 = !wi *. wpr in
      let q4 = wtemp *. wpi in
      wi := q3 +. q4 +. !wi;
      i3 := !i3 + 2
    done
  done;
  if isign = -1 then fourn data nn isign

let clamp2m v = if v <= 0 then 0 else min v 2000000

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
  let is2d = op = "RFFT2D" || op = "IRFFT2D" in
  let is3d = op = "RFFT3D" || op = "IRFFT3D" in
  if is2d || is3d then (
    let n1, n2, n3, base =
      if is2d then (1, int_of_string toks.(1), int_of_string toks.(2), 3)
      else
        ( int_of_string toks.(1),
          int_of_string toks.(2),
          int_of_string toks.(3),
          4 )
    in
    let full_data =
      if n1 <= 0 || n2 <= 0 || n3 <= 0 then 0 else n1 * n2 * n3
    in
    let full_speq = if n1 <= 0 || n2 <= 0 then 0 else n1 * 2 * n2 in
    let half3 = if n3 >= 2 then n3 lsr 1 else 0 in
    let ntot = if n1 <= 0 || n2 <= 0 then 0 else n1 * n2 * half3 in
    let valid =
      ispow2 n1 && ispow2 n2 && n3 >= 2
      && n3 land (n3 - 1) = 0
      && full_data <= 2000000
      && full_speq <= 2000000 && ntot >= 2 && ntot land (ntot - 1) = 0
    in
    let ndata = if valid then full_data else clamp2m full_data in
    let nspeq = if valid then full_speq else clamp2m full_speq in
    let inverse = op = "IRFFT2D" || op = "IRFFT3D" in
    let data = Array.init ndata (fun i -> float_of_string toks.(base + i)) in
    let speq =
      Array.init nspeq (fun i ->
          if inverse then float_of_string toks.(base + ndata + i) else 0.0)
    in
    let ok =
      valid
      &&
      try
        rlft3 data speq (if inverse then -1 else 1) n1 n2 n3;
        true
      with BadN -> false
    in
    let vals =
      (if ok then Array.to_list (Array.map (fun v -> fmt6 (canon v)) data)
       else List.init ndata (fun _ -> "0.000000"))
      @ (if ok && not inverse then
           Array.to_list (Array.map (fun v -> fmt6 (canon v)) speq)
         else if (not ok) && not inverse then
           List.init nspeq (fun _ -> "0.000000")
         else [])
      @ [if ok then "0" else "1"]
    in
    print_endline (String.concat " " vals))
