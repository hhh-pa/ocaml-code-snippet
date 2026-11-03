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
    (((((a1 *. r +. a2) *. r +. a3) *. r +. a4) *. r +. a5) *. r +. a6) *. q /. (((((b1 *. r +. b2) *. r +. b3) *. r +. b4) *. r +. b5) *. r +. 1.0)
  else
    let q = sqrt (-2.0 *. log (1.0 -. p)) in
    -.(((((c1 *. q +. c2) *. q +. c3) *. q +. c4) *. q +. c5) *. q +. c6) /. (((((d1 *. q +. d2) *. q +. d3) *. q +. d4) *. q +. 1.0))

let tlog a b =
  if b = 0.0 then 0.0 else b *. log a

let pi_const = 3.14159265358979323846

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
  if Array.length tokens >= 3 then begin
    let pos = ref 0 in
    let next_str () = let t = tokens.(!pos) in incr pos; t in
    let next_int () = int_of_string (next_str ()) in
    let next_float () = float_of_string (next_str ()) in
    let m = next_int () in
    let alpha = next_float () in
    let mcn = next_int () in
    let r = Array.init m (fun _ -> next_float ()) in
    let fm = float_of_int m in
    let sum = ref 0.0 in
    for i = 0 to m - 1 do
      sum := !sum +. r.(i)
    done;
    let mu = !sum /. fm in
    let vv = ref 0.0 in
    for i = 0 to m - 1 do
      let d = r.(i) -. mu in
      vv := !vv +. d *. d
    done;
    let vv = !vv /. (fm -. 1.0) in
    let sd = sqrt vv in
    let z = phiinv alpha in
    let pvar = -. (mu +. sd *. z) in
    let pes = sd *. exp (-. z *. z /. 2.0) /. sqrt (2.0 *. pi_const) /. alpha -. mu in
    let s = Array.copy r in
    Array.sort compare s;
    let idx = max 0 (int_of_float (ceil (alpha *. fm)) - 1) in
    let hvar = -. s.(idx) in
    let hes = ref 0.0 in
    for i = 0 to idx do
      hes := !hes +. s.(i)
    done;
    let hes = -. !hes /. float_of_int (idx + 1) in
    let lcg = ref 42L in
    let a_const = 6364136223846793005L in
    let c_const = 1442695040888963407L in
    let uni () =
      lcg := Int64.add (Int64.mul !lcg a_const) c_const;
      let shifted = Int64.add (Int64.shift_right_logical !lcg 11) 1L in
      Int64.to_float shifted /. 9007199254740992.0
    in
    let sim = Array.make mcn 0.0 in
    let sim_len = ref 0 in
    while !sim_len < mcn do
      let u1 = uni () in
      let u2 = uni () in
      let rad = sqrt (-2.0 *. log u1) in
      sim.(!sim_len) <- mu +. sd *. rad *. cos (2.0 *. pi_const *. u2);
      incr sim_len;
      if !sim_len < mcn then begin
        sim.(!sim_len) <- mu +. sd *. rad *. sin (2.0 *. pi_const *. u2);
        incr sim_len
      end
    done;
    Array.sort compare sim;
    let j = max 0 (int_of_float (ceil (alpha *. float_of_int mcn)) - 1) in
    let mvar = -. sim.(j) in
    let mes = ref 0.0 in
    for i = 0 to j do
      mes := !mes +. sim.(i)
    done;
    let mes = -. !mes /. float_of_int (j + 1) in
    let hit = Array.make m 0 in
    for i = 0 to m - 1 do
      hit.(i) <- (if r.(i) < -. hvar then 1 else 0)
    done;
    let x = ref 0 in
    for i = 0 to m - 1 do
      x := !x + hit.(i)
    done;
    let fx = float_of_int !x in
    let kup =
      -. 2.0 *. (tlog (1.0 -. alpha) (fm -. fx) +. tlog alpha fx)
      +. 2.0 *. (tlog (1.0 -. fx /. fm) (fm -. fx) +. tlog (fx /. fm) fx)
    in
    let n00 = ref 0 in
    let n01 = ref 0 in
    let n10 = ref 0 in
    let n11 = ref 0 in
    for i = 1 to m - 1 do
      if hit.(i - 1) = 0 && hit.(i) = 0 then incr n00
      else if hit.(i - 1) = 0 && hit.(i) = 1 then incr n01
      else if hit.(i - 1) = 1 && hit.(i) = 0 then incr n10
      else incr n11
    done;
    let fn00 = float_of_int !n00 in
    let fn01 = float_of_int !n01 in
    let fn10 = float_of_int !n10 in
    let fn11 = float_of_int !n11 in
    let pi0 = if fn00 +. fn01 > 0.0 then fn01 /. (fn00 +. fn01) else 0.0 in
    let pi1 = if fn10 +. fn11 > 0.0 then fn11 /. (fn10 +. fn11) else 0.0 in
    let pi = (fn01 +. fn11) /. (fm -. 1.0) in
    let chr_val =
      -. 2.0 *. (tlog (1.0 -. pi) (fn00 +. fn10) +. tlog pi (fn01 +. fn11))
      +. 2.0 *. (tlog (1.0 -. pi0) fn00 +. tlog pi0 fn01 +. tlog (1.0 -. pi1) fn10 +. tlog pi1 fn11)
    in
    Printf.printf "PVAR: %.6f PES: %.6f\n" pvar pes;
    Printf.printf "HVAR: %.6f HES: %.6f\n" hvar hes;
    Printf.printf "MVAR: %.6f MES: %.6f\n" mvar mes;
    Printf.printf "KUPIEC: %.6f\n" kup;
    Printf.printf "CHRIST: %.6f\n" chr_val
  end
