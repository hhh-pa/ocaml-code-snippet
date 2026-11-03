let pi = 3.14159265358979323846

let erf7 x =
  let a1 = 0.254829592 and a2 = -0.284496736 and a3 = 1.421413741
  and a4 = -1.453152027 and a5 = 1.061405429 and p = 0.3275911 in
  let s = if x < 0.0 then -1.0 else 1.0 in
  let ax = abs_float x in
  let t = 1.0 /. (1.0 +. p *. ax) in
  let poly = (((((a5 *. t +. a4) *. t +. a3) *. t +. a2) *. t +. a1)) in
  s *. (1.0 -. poly *. t *. exp (-. ax *. ax))

let ncdf x = 0.5 *. (1.0 +. erf7 (x /. sqrt 2.0))

let phiinv p =
  let a1 = -3.969683028665376e+01 and a2 = 2.209460984245205e+02 and a3 = -2.759285104469687e+02
  and a4 = 1.383577518672690e+02 and a5 = -3.066479806614716e+01 and a6 = 2.506628277459239e+00 in
  let b1 = -5.447609879822406e+01 and b2 = 1.615858368580409e+02 and b3 = -1.556989798598866e+02
  and b4 = 6.680131188771972e+01 and b5 = -1.328068155288572e+01 in
  let c1 = -7.784894002430293e-03 and c2 = -3.223964580411365e-01 and c3 = -2.400758277161838e+00
  and c4 = -2.549732539343734e+00 and c5 = 4.374664141464968e+00 and c6 = 2.938163982698783e+00 in
  let d1 = 7.784695709041462e-03 and d2 = 3.224671290700398e-01 and d3 = 2.445134137142996e+00 and d4 = 3.754408661907416e+00 in
  if p < 0.02425 then
    let q = sqrt (-2.0 *. log p) in
    (((((c1 *. q +. c2) *. q +. c3) *. q +. c4) *. q +. c5) *. q +. c6) /. (((((d1 *. q +. d2) *. q +. d3) *. q +. d4) *. q +. 1.0))
  else if p <= 0.97575 then
    let q = p -. 0.5 in
    let r = q *. q in
    (((((a1 *. r +. a2) *. r +. a3) *. r +. a4) *. r +. a5) *. r +. a6) *. q /. ((((((b1 *. r +. b2) *. r +. b3) *. r +. b4) *. r +. b5) *. r +. 1.0))
  else
    let q = sqrt (-2.0 *. log (1.0 -. p)) in
    -. (((((c1 *. q +. c2) *. q +. c3) *. q +. c4) *. q +. c5) *. q +. c6) /. (((((d1 *. q +. d2) *. q +. d3) *. q +. d4) *. q +. 1.0))

let () =
  let tokens =
    let buf = Buffer.create 4096 in
    (try while true do Buffer.add_channel buf stdin 4096 done with End_of_file -> ());
    let s = Buffer.contents buf in
    let len = String.length s in
    let res = ref [] in
    let i = ref 0 in
    while !i < len do
      while !i < len && (s.[!i] = ' ' || s.[!i] = '\t' || s.[!i] = '\r' || s.[!i] = '\n') do incr i done;
      if !i < len then begin
        let st = !i in
        while !i < len && not (s.[!i] = ' ' || s.[!i] = '\t' || s.[!i] = '\r' || s.[!i] = '\n') do incr i done;
        res := String.sub s st (!i - st) :: !res
      end
    done;
    Array.of_list (List.rev !res)
  in
  if Array.length tokens >= 5 then begin
    let pos = ref 0 in
    let next_str () = let t = tokens.(!pos) in incr pos; t in
    let next_float () = float_of_string (next_str ()) in
    let e_val = next_float () in
    let se = next_float () in
    let d_val = next_float () in
    let rr = next_float () in
    let t_val = next_float () in
    let sa = ref (se *. e_val /. (e_val +. d_val)) in
    let a_val = ref (e_val +. d_val *. exp (-. rr *. t_val)) in
    let sqrt_t = sqrt t_val in
    for _it = 0 to 99 do
      for _nt = 0 to 49 do
        let d1 = (log (!a_val /. d_val) +. (rr +. 0.5 *. !sa *. !sa) *. t_val) /. (!sa *. sqrt_t) in
        let d2 = d1 -. !sa *. sqrt_t in
        let c = !a_val *. ncdf d1 -. d_val *. exp (-. rr *. t_val) *. ncdf d2 in
        let delta = ncdf d1 in
        a_val := !a_val -. (c -. e_val) /. delta
      done;
      let d1 = (log (!a_val /. d_val) +. (rr +. 0.5 *. !sa *. !sa) *. t_val) /. (!sa *. sqrt_t) in
      sa := se *. e_val /. (ncdf d1 *. !a_val)
    done;
    let dd = (log (!a_val /. d_val) +. (rr -. 0.5 *. !sa *. !sa) *. t_val) /. (!sa *. sqrt_t) in
    let pd = ncdf (-. dd) in
    let p = next_float () in
    let rho = next_float () in
    let att = next_float () in
    let det = next_float () in
    let gx = [|0.3429013272237046; 1.036610829789514; 1.756683649299882; 2.53273167423279; 3.436159118837738|] in
    let gw = [|0.6108626337353258; 0.2401386110823147; 0.03387439445548106; 0.001343645746781232; 7.64043285523262e-06|] in
    let pinv = phiinv p in
    let sqr = sqrt rho in
    let sq1 = sqrt (1.0 -. rho) in
    let acc = ref 0.0 in
    for i = 0 to 4 do
      let s_vals = [-1.0; 1.0] in
      List.iter (fun s ->
        let z = s *. sqrt 2.0 *. gx.(i) in
        let p_cond = ncdf ((pinv -. sqr *. z) /. sq1) in
        let l_val = (p_cond -. att) /. (det -. att) in
        let l_val = if l_val < 0.0 then 0.0 else if l_val > 1.0 then 1.0 else l_val in
        acc := !acc +. gw.(i) *. l_val
      ) s_vals
    done;
    let el = !acc /. sqrt pi in
    Printf.printf "MERTON: %.6f %.6f %.6f %.6f\n" (!a_val +. 0.0) (!sa +. 0.0) (dd +. 0.0) (pd +. 0.0);
    Printf.printf "VASICEK: %.6f\n" (el +. 0.0)
  end
