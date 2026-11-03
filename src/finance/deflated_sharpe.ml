let aerf x =
  let s = if x < 0.0 then -1.0 else 1.0 in
  let ax = abs_float x in
  let t = 1.0 /. (1.0 +. 0.3275911 *. ax) in
  let y = 1.0 -. (((((1.061405429 *. t -. 1.453152027) *. t +. 1.421413741) *. t -. 0.284496736) *. t +. 0.254829592) *. t *. exp (-. ax *. ax)) in
  s *. y

let phi x = 0.5 *. (1.0 +. aerf (x /. sqrt 2.0))

let phi_inv p =
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
  else if p > 0.97575 then
    let q = sqrt (-2.0 *. log (1.0 -. p)) in
    -. (((((c1 *. q +. c2) *. q +. c3) *. q +. c4) *. q +. c5) *. q +. c6) /. (((((d1 *. q +. d2) *. q +. d3) *. q +. d4) *. q +. 1.0))
  else
    let q = p -. 0.5 in
    let r = q *. q in
    (((((a1 *. r +. a2) *. r +. a3) *. r +. a4) *. r +. a5) *. r +. a6) *. q /. ((((((b1 *. r +. b2) *. r +. b3) *. r +. b4) *. r +. b5) *. r +. 1.0))

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
  if Array.length tokens >= 3 then begin
    let pos = ref 0 in
    let next_str () = let t = tokens.(!pos) in incr pos; t in
    let next_int () = int_of_string (next_str ()) in
    let next_float () = float_of_string (next_str ()) in
    let n = next_int () in
    let k = next_int () in
    let bench = next_float () in
    let r = Array.init n (fun _ -> next_float ()) in
    let tr = Array.init k (fun _ -> next_float ()) in
    let m = (Array.fold_left (+.) 0.0 r) /. float_of_int n in
    let v = ref 0.0 in
    let m2 = ref 0.0 in
    let m3 = ref 0.0 in
    let m4 = ref 0.0 in
    for i = 0 to n - 1 do
      let d = r.(i) -. m in
      v := !v +. d *. d;
      m2 := !m2 +. d *. d;
      m3 := !m3 +. d *. d *. d;
      m4 := !m4 +. d *. d *. d *. d
    done;
    let v = !v /. float_of_int (n - 1) in
    let m2 = !m2 /. float_of_int n in
    let m3 = !m3 /. float_of_int n in
    let m4 = !m4 /. float_of_int n in
    let sd = sqrt v in
    let sr = m /. sd *. sqrt 252.0 in
    let g3 = m3 /. (m2 ** 1.5) in
    let g4 = m4 /. (m2 *. m2) in
    let den = ref (1.0 -. g3 *. sr +. (g4 -. 1.0) /. 4.0 *. sr *. sr) in
    if !den <= 0.0 then den := 1e-12;
    let sc = sqrt (float_of_int (n - 1)) /. sqrt !den in
    let psr = phi ((sr -. bench) *. sc) in
    let tm = (Array.fold_left (+.) 0.0 tr) /. float_of_int k in
    let var_tr = ref 0.0 in
    if k > 1 then begin
      for i = 0 to k - 1 do
        let d = tr.(i) -. tm in
        var_tr := !var_tr +. d *. d
      done;
      var_tr := !var_tr /. float_of_int (k - 1)
    end;
    let sr0 =
      if k >= 2 then
        let g = 0.5772156649015329 in
        let e = 2.718281828459045 in
        let a1 = phi_inv (1.0 -. 1.0 /. float_of_int k) in
        let a2 = phi_inv (1.0 -. 1.0 /. (float_of_int k *. e)) in
        sqrt !var_tr *. ((1.0 -. g) *. a1 +. g *. a2)
      else
        0.0
    in
    let dsr = phi ((sr -. sr0) *. sc) in
    Printf.printf "SR: %.6f\n" (sr +. 0.0);
    Printf.printf "SKEW: %.6f KURT: %.6f\n" (g3 +. 0.0) (g4 +. 0.0);
    Printf.printf "PSR: %.6f\n" (psr +. 0.0);
    Printf.printf "DSR: %.6f SR0: %.6f\n" (dsr +. 0.0) (sr0 +. 0.0)
  end
