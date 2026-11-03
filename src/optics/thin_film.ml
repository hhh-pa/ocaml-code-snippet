open Complex

let sn x =
  let v = x +. 0.0 in
  if abs_float v < 5e-7 then 0.0 else v

let norm_sq c =
  c.re *. c.re +. c.im *. c.im

let pi = acos (-1.0)

let () =
  try
    let l_cnt = Scanf.scanf " %d" (fun x -> x) in
    let lambda_val = Scanf.scanf " %f" (fun x -> x) in
    let theta0 = Scanf.scanf " %f" (fun x -> x) in
    let n0 = Scanf.scanf " %f" (fun x -> x) in
    let ns = Scanf.scanf " %f" (fun x -> x) in

    let nn = Array.make l_cnt 0.0 in
    let dd = Array.make l_cnt 0.0 in
    for j = 0 to l_cnt - 1 do
      nn.(j) <- Scanf.scanf " %f" (fun x -> x);
      dd.(j) <- Scanf.scanf " %f" (fun x -> x)
    done;

    let s0 = n0 *. sin (theta0 *. pi /. 180.0) in

    for pol = 0 to 1 do
      let m11 = ref { re = 1.0; im = 0.0 } in
      let m12 = ref zero in
      let m21 = ref zero in
      let m22 = ref { re = 1.0; im = 0.0 } in

      for j = 0 to l_cnt - 1 do
        let st_raw = s0 /. nn.(j) in
        let st = if st_raw > 1.0 then 1.0 else if st_raw < -1.0 then -1.0 else st_raw in
        let ct = Float.sqrt (max 0.0 (1.0 -. st *. st)) in
        let eta = if pol = 0 then nn.(j) *. ct else nn.(j) /. ct in
        let delta = 2.0 *. pi *. nn.(j) *. dd.(j) *. ct /. lambda_val in
        let c_val = cos delta in
        let s_val = sin delta in
        let iu = { re = 0.0; im = 1.0 } in
        let n11 = { re = c_val; im = 0.0 } in
        let n12 = div (mul iu { re = s_val; im = 0.0 }) { re = eta; im = 0.0 } in
        let n21 = mul (mul iu { re = eta; im = 0.0 }) { re = s_val; im = 0.0 } in
        let n22 = { re = c_val; im = 0.0 } in
        let o11 = add (mul !m11 n11) (mul !m12 n21) in
        let o12 = add (mul !m11 n12) (mul !m12 n22) in
        let o21 = add (mul !m21 n11) (mul !m22 n21) in
        let o22 = add (mul !m21 n12) (mul !m22 n22) in
        m11 := o11;
        m12 := o12;
        m21 := o21;
        m22 := o22
      done;

      let st0 = sin (theta0 *. pi /. 180.0) in
      let ct0 = cos (theta0 *. pi /. 180.0) in
      let sts_raw = s0 /. ns in
      let sts = if sts_raw > 1.0 then 1.0 else if sts_raw < -1.0 then -1.0 else sts_raw in
      let cts = sqrt (max 0.0 (1.0 -. sts *. sts)) in
      let e0 = if pol = 0 then n0 *. ct0 else n0 /. ct0 in
      let es = if pol = 0 then ns *. cts else ns /. cts in

      let term1 = mul { re = e0; im = 0.0 } !m11 in
      let term2 = mul { re = e0 *. es; im = 0.0 } !m12 in
      let term3 = !m21 in
      let term4 = mul { re = es; im = 0.0 } !m22 in

      let den = add (add (add term1 term2) term3) term4 in
      let num_r = sub (sub (add term1 term2) term3) term4 in
      let r = div num_r den in
      let t = div { re = 2.0 *. e0; im = 0.0 } den in
      let r_val = norm_sq r in
      let t_val = (es /. e0) *. norm_sq t in

      let prefix = if pol = 0 then "S: " else "P: " in
      Printf.printf "%sr %.6f %.6f R %.6f T %.6f\n" prefix (sn r.re) (sn r.im) (sn r_val) (sn t_val)
    done
  with End_of_file -> ()
