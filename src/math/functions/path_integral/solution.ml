(* Hypergeometric 2F1 by path integration (NR 5.14). Port of the C++ reference. *)

let fmt6 v =
  let w = if abs_float v < 5e-7 then 0.0 else v in
  Printf.sprintf "%.6f" (w +. 0.0)

let canon v = if abs_float v < 5e-7 then 0.0 else v

let deriv a b c zr zi dzr dzi fr fi gr gi =
  let dfr = dzr *. gr -. dzi *. gi in
  let dfi = dzr *. gi +. dzi *. gr in
  let abr = a *. b in
  let kr = c -. (a +. b +. 1.0) *. zr in
  let ki = (-.(a +. b +. 1.0)) *. zi in
  let t1r = abr *. fr in
  let t1i = abr *. fi in
  let t2r = kr *. gr -. ki *. gi in
  let t2i = kr *. gi +. ki *. gr in
  let nr = t1r -. t2r in
  let ni = t1i -. t2i in
  let dr = zr *. (1.0 -. zr) +. zi *. zi in
  let di = zi *. (1.0 -. zr) -. zr *. zi in
  let dd = dr *. dr +. di *. di in
  let qr = (nr *. dr +. ni *. di) /. dd in
  let qi = (ni *. dr -. nr *. di) /. dd in
  (dfr, dfi, dzr *. qr -. dzi *. qi, dzr *. qi +. dzi *. qr)

let series a b c zr zi =
  let pr = ref 1.0 and pi = ref 0.0 in
  let fr = ref 1.0 and fi = ref 0.0 in
  let gr = ref 0.0 and gi = ref 0.0 in
  for j = 1 to 60 do
    let fj = float j in
    let f = (a +. fj -. 1.0) *. (b +. fj -. 1.0) /. ((c +. fj -. 1.0) *. fj) in
    let jf = fj *. f in
    gr := !gr +. jf *. !pr;
    gi := !gi +. jf *. !pi;
    let nr = (!pr *. zr -. !pi *. zi) *. f in
    let ni = (!pr *. zi +. !pi *. zr) *. f in
    pr := nr;
    pi := ni;
    fr := !fr +. !pr;
    fi := !fi +. !pi
  done;
  (!fr, !fi, !gr, !gi)

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
  let a = float_of_string toks.(0) in
  let b = float_of_string toks.(1) in
  let c = float_of_string toks.(2) in
  let zr = float_of_string toks.(3) in
  let zi = float_of_string toks.(4) in
  let fr = ref 0.0 and fi = ref 0.0 in
  let r = sqrt (zr *. zr +. zi *. zi) in
  (if r <= 0.5 then (
     let s0, s1, _, _ = series a b c zr zi in
     fr := s0;
     fi := s1)
   else (
     let z0r, z0i =
       if zr < 0.0 then (-0.5, 0.0)
       else if zr <= 1.0 then (0.5, 0.0)
       else if zi >= 0.0 then (0.0, 0.5)
       else (0.0, -0.5)
     in
     let s0, s1, s2, s3 = series a b c z0r z0i in
     let gr = ref s2 and gi = ref s3 in
     fr := s0;
     fi := s1;
     let dzr = zr -. z0r and dzi = zi -. z0i in
     let h = 1.0 /. 800.0 in
     let cr = ref z0r and ci = ref z0i in
     for _st = 0 to 799 do
       let k1fr, k1fi, k1gr, k1gi = deriv a b c !cr !ci dzr dzi !fr !fi !gr !gi in
       let mr = !cr +. 0.5 *. h *. dzr and mi = !ci +. 0.5 *. h *. dzi in
       let k2fr, k2fi, k2gr, k2gi =
         deriv a b c mr mi dzr dzi (!fr +. 0.5 *. h *. k1fr) (!fi +. 0.5 *. h *. k1fi)
           (!gr +. 0.5 *. h *. k1gr) (!gi +. 0.5 *. h *. k1gi)
       in
       let k3fr, k3fi, k3gr, k3gi =
         deriv a b c mr mi dzr dzi (!fr +. 0.5 *. h *. k2fr) (!fi +. 0.5 *. h *. k2fi)
           (!gr +. 0.5 *. h *. k2gr) (!gi +. 0.5 *. h *. k2gi)
       in
       let er = !cr +. h *. dzr and ei = !ci +. h *. dzi in
       let k4fr, k4fi, k4gr, k4gi =
         deriv a b c er ei dzr dzi (!fr +. h *. k3fr) (!fi +. h *. k3fi) (!gr +. h *. k3gr)
           (!gi +. h *. k3gi)
       in
       fr := !fr +. h *. (k1fr +. 2.0 *. k2fr +. 2.0 *. k3fr +. k4fr) /. 6.0;
       fi := !fi +. h *. (k1fi +. 2.0 *. k2fi +. 2.0 *. k3fi +. k4fi) /. 6.0;
       gr := !gr +. h *. (k1gr +. 2.0 *. k2gr +. 2.0 *. k3gr +. k4gr) /. 6.0;
       gi := !gi +. h *. (k1gi +. 2.0 *. k2gi +. 2.0 *. k3gi +. k4gi) /. 6.0;
       cr := er;
       ci := ei
     done));
  print_string (fmt6 (canon !fr));
  print_string " ";
  print_string (fmt6 (canon !fi));
  print_newline ()
