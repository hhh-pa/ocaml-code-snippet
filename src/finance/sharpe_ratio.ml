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

let norm_cdf x = 0.5 *. (1.0 +. erf (x /. sqrt 2.0))

let () =
  let tokens =
    let buf = Buffer.create 4096 in
    (try while true do Buffer.add_channel buf stdin 4096 done with End_of_file -> ());
    let s = Buffer.contents buf in
    let len = String.length s in
    let res = ref [] in
    let i = ref 0 in
    while !i < len do
      while !i < len && (let c = s.[!i] in c = ' ' || c = '\t' || c = '\r' || c = '\n') do incr i done;
      if !i < len then begin
        let st = !i in
        while !i < len && (let c = s.[!i] in c <> ' ' && c <> '\t' && c <> '\r' && c <> '\n') do incr i done;
        res := String.sub s st (!i - st) :: !res
      end
    done;
    Array.of_list (List.rev !res)
  in
  if Array.length tokens >= 2 then begin
    let pos = ref 0 in
    let next_str () = let t = tokens.(!pos) in incr pos; t in
    let next_int () = int_of_string (next_str ()) in
    let next_float () = float_of_string (next_str ()) in
    let n = next_int () in
    let rets = Array.init n (fun _ -> next_float ()) in
    let rf = next_float () in
    let sr_benchmark = if !pos < Array.length tokens then next_float () else 0.0 in
    let fn = float_of_int n in
    let sum = ref 0.0 in
    for i = 0 to n - 1 do
      sum := !sum +. rets.(i)
    done;
    let mu = !sum /. fn in
    let excess_mu = mu -. rf in
    let var = ref 0.0 in
    for i = 0 to n - 1 do
      let d = rets.(i) -. mu in
      var := !var +. d *. d
    done;
    let var = !var /. (fn -. 1.0) in
    let sd = sqrt var in
    let sr = if sd = 0.0 then 0.0 else excess_mu /. sd *. sqrt 252.0 in
    let m2 = ref 0.0 in
    let m3 = ref 0.0 in
    let m4 = ref 0.0 in
    for i = 0 to n - 1 do
      let d = rets.(i) -. mu in
      let d2 = d *. d in
      let d3 = d2 *. d in
      let d4 = d2 *. d2 in
      m2 := !m2 +. d2;
      m3 := !m3 +. d3;
      m4 := !m4 +. d4
    done;
    let m2 = !m2 /. fn in
    let m3 = !m3 /. fn in
    let m4 = !m4 /. fn in
    let skew = if m2 = 0.0 then 0.0 else m3 /. (m2 ** 1.5) in
    let kurt = if m2 = 0.0 then 0.0 else m4 /. (m2 *. m2) in
    let numer = (sr -. sr_benchmark) *. sqrt (fn -. 1.0) in
    let denom2 = 1.0 -. skew *. sr +. (kurt -. 1.0) /. 4.0 *. sr *. sr in
    let denom2 = if denom2 < 0.0 then 1.0 else denom2 in
    let denom = sqrt denom2 in
    let psr = norm_cdf (numer /. denom) in
    Printf.printf "%.4f\n%.4f\n" sr psr
  end
