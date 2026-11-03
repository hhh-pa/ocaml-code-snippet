let rec ext_gcd a b =
  if b = 0L then (a, 1L, 0L)
  else
    let q = Int64.div a b in
    let r = Int64.rem a b in
    let (g, x1, y1) = ext_gcd b r in
    (g, y1, Int64.sub x1 (Int64.mul q y1))

let () =
  try
    let k = Scanf.scanf " %d" (fun x -> x) in
    if k <= 0 then exit 0;
    let r = Array.make k 0L in
    let m = Array.make k 0L in
    for i = 0 to k - 1 do
      r.(i) <- Scanf.scanf " %Ld" (fun x -> x);
      m.(i) <- Scanf.scanf " %Ld" (fun x -> x)
    done;

    let r1 = ref r.(0) in
    let m1 = ref m.(0) in
    let ok = ref true in

    let i = ref 1 in
    while !i < k && !ok do
      let r2 = r.(!i) in
      let m2 = m.(!i) in
      let (g, p, _) = ext_gcd !m1 m2 in
      let diff = Int64.sub r2 !r1 in
      if Int64.rem diff g <> 0L then
        ok := false
      else begin
        let m2_reduced = Int64.div m2 g in
        let diff_div_g = Int64.div diff g in
        let factor = Int64.rem diff_div_g m2_reduced in
        let p_rem = Int64.rem p m2_reduced in
        let k1 = ref (Int64.rem (Int64.mul p_rem factor) m2_reduced) in
        if !k1 < 0L then k1 := Int64.add !k1 m2_reduced;

        let new_m = Int64.mul !m1 m2_reduced in
        let new_r = ref (Int64.rem (Int64.add !r1 (Int64.mul !m1 !k1)) new_m) in
        if !new_r < 0L then new_r := Int64.add !new_r new_m;

        r1 := !new_r;
        m1 := new_m
      end;
      incr i
    done;

    if !ok then
      Printf.printf "%Ld %Ld\n" !r1 !m1
    else
      Printf.printf "NO SOLUTION\n"
  with End_of_file -> ()
