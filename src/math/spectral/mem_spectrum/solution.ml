(* Maximum-entropy (all-poles) power spectrum from Burg LP
   coefficients by evaluating the prediction-error filter on the
   unit circle.
   Reference: Press et al., Numerical Recipes 3e, Section 13.8
   (evlmem; takes memcof-style (d, xms) as explicit input).
   Documented decisions: the book's literal 6.28318530717959 is
   used verbatim for theta; a zero denominator is unguarded in
   the book (fixtures keep it positive).
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

let evlmem fdt d xms =
  let m = Array.length d in
  let sumr = ref 1.0 and sumi = ref 0.0 in
  let wr = ref 1.0 and wi = ref 0.0 in
  let theta = 6.28318530717959 *. fdt in
  let wtemp = sin (0.5 *. theta) in
  let wpr = (-2.0 *. wtemp) *. wtemp in
  let wpi = sin theta in
  for i = 0 to m - 1 do
    let wtemp = !wr in
    let q1 = wtemp *. wpr in
    let q2 = !wi *. wpi in
    wr := q1 -. q2 +. !wr;
    let q3 = !wi *. wpr in
    let q4 = wtemp *. wpi in
    wi := q3 +. q4 +. !wi;
    let r1 = d.(i) *. !wr in
    sumr := !sumr -. r1;
    let r2 = d.(i) *. !wi in
    sumi := !sumi -. r2
  done;
  let s1 = !sumr *. !sumr in
  let s2 = !sumi *. !sumi in
  xms /. (s1 +. s2)

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
  if op = "MEMSPEC" then (
    let m = int_of_string toks.(1) and k = int_of_string toks.(2) in
    let mm = max 0 m and kk = max 0 k in
    let d = Array.init mm (fun i -> float_of_string toks.(3 + i)) in
    let xms = float_of_string toks.(3 + mm) in
    let parts = ref [] in
    for i = 0 to kk - 1 do
      let f = float_of_string toks.(4 + mm + i) in
      parts := fmt6 (canon (evlmem f d xms)) :: !parts
    done;
    parts := "0" :: !parts;
    print_endline (String.concat " " (List.rev !parts)))
