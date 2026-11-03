let rec ext_gcd a b =
  if b = 0L then (a, 1L, 0L)
  else
    let q = Int64.div a b in
    let r = Int64.rem a b in
    let (g, x1, y1) = ext_gcd b r in
    (g, y1, Int64.sub x1 (Int64.mul q y1))

let mod_inv a m =
  let (_, x, _) = ext_gcd a m in
  let res = Int64.rem x m in
  if res < 0L then Int64.add res m else res

let mod_pow base exp m =
  let rec loop b e acc =
    if e = 0L then acc
    else
      let new_acc = if Int64.rem e 2L = 1L then Int64.rem (Int64.mul acc b) m else acc in
      let new_b = Int64.rem (Int64.mul b b) m in
      loop new_b (Int64.div e 2L) new_acc
  in
  loop (Int64.rem base m) exp 1L

let () =
  try
    let p = Scanf.scanf " %Ld" (fun x -> x) in
    let q = Scanf.scanf " %Ld" (fun x -> x) in
    let e = Scanf.scanf " %Ld" (fun x -> x) in
    let m = Scanf.scanf " %Ld" (fun x -> x) in

    let n = Int64.mul p q in
    let p_minus_1 = Int64.sub p 1L in
    let q_minus_1 = Int64.sub q 1L in
    let phi = Int64.mul p_minus_1 q_minus_1 in
    let d = mod_inv e phi in
    let c = mod_pow m e n in

    let dp = Int64.rem d p_minus_1 in
    let dq = Int64.rem d q_minus_1 in
    let qinv = mod_inv q p in
    let m1 = mod_pow c dp p in
    let m2 = mod_pow c dq q in
    let diff = Int64.rem (Int64.sub m1 m2) p in
    let diff_pos = if diff < 0L then Int64.add diff p else diff in
    let h = Int64.rem (Int64.mul qinv diff_pos) p in
    let h_pos = if h < 0L then Int64.add h p else h in
    let decrypted = Int64.add m2 (Int64.mul h_pos q) in

    Printf.printf "%Ld %Ld %Ld\n" n phi d;
    Printf.printf "%Ld\n" c;
    Printf.printf "%Ld\n" decrypted
  with End_of_file -> ()
