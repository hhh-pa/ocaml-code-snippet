let erf x =
  let a1 = 0.254829592
  and a2 = -0.284496736
  and a3 = 1.421413741
  and a4 = -1.453152027
  and a5 = 1.061405429
  and p = 0.3275911 in
  let sign = if x < 0.0 then -1.0 else 1.0 in
  let ax = abs_float x in
  let t = 1.0 /. (1.0 +. p *. ax) in
  let poly = ((((a5 *. t +. a4) *. t +. a3) *. t +. a2) *. t +. a1) in
  let y = 1.0 -. poly *. t *. exp (-. ax *. ax) in
  sign *. y

let pi = 3.14159265358979323846
let norm_cdf x = 0.5 *. (1.0 +. erf (x /. sqrt 2.0))
let norm_pdf x = exp (-0.5 *. x *. x) /. sqrt (2.0 *. pi)

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
  if Array.length tokens >= 6 then begin
    let s_val = float_of_string tokens.(0) in
    let k = float_of_string tokens.(1) in
    let t = float_of_string tokens.(2) in
    let r = float_of_string tokens.(3) in
    let sig_val = float_of_string tokens.(4) in
    let opt_type = tokens.(5) in
    let sqrt_t = sqrt t in
    let d1 = (log (s_val /. k) +. (r +. 0.5 *. sig_val *. sig_val) *. t) /. (sig_val *. sqrt_t) in
    let d2 = d1 -. sig_val *. sqrt_t in
    let price, delta, rho =
      if opt_type = "call" then
        let p = s_val *. norm_cdf d1 -. k *. exp (-. r *. t) *. norm_cdf d2 in
        let d = norm_cdf d1 in
        let rh = k *. t *. exp (-. r *. t) *. norm_cdf d2 in
        (p, d, rh)
      else
        let p = k *. exp (-. r *. t) *. norm_cdf (-. d2) -. s_val *. norm_cdf (-. d1) in
        let d = norm_cdf d1 -. 1.0 in
        let rh = -. k *. t *. exp (-. r *. t) *. norm_cdf (-. d2) in
        (p, d, rh)
    in
    let gamma = norm_pdf d1 /. (s_val *. sig_val *. sqrt_t) in
    let vega = s_val *. norm_pdf d1 *. sqrt_t in
    let term1 = -. (s_val *. norm_pdf d1 *. sig_val) /. (2.0 *. sqrt_t) in
    let theta =
      if opt_type = "call" then
        term1 -. r *. k *. exp (-. r *. t) *. norm_cdf d2
      else
        term1 +. r *. k *. exp (-. r *. t) *. norm_cdf (-. d2)
    in
    Printf.printf "%.4f\n" (price +. 0.0);
    Printf.printf "%.4f\n" (delta +. 0.0);
    Printf.printf "%.4f\n" (gamma +. 0.0);
    Printf.printf "%.4f\n" (vega +. 0.0);
    Printf.printf "%.4f\n" (theta +. 0.0);
    Printf.printf "%.4f\n" (rho +. 0.0)
  end
