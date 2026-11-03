let rec mod_exp b e m =
  if e = 0L then 1L
  else
    let half = mod_exp b (Int64.shift_right e 1) m in
    let res = Int64.rem (Int64.mul half half) m in
    if Int64.logand e 1L = 1L then
      Int64.rem (Int64.mul res b) m
    else
      res

let ext_gcd a b =
  let rec loop a b x0 x1 y0 y1 =
    if b = 0L then (a, x0, y0)
    else
      let q = Int64.div a b in
      loop b (Int64.rem a b) x1 (Int64.sub x0 (Int64.mul q x1)) y1 (Int64.sub y0 (Int64.mul q y1))
  in
  loop a b 1L 0L 0L 1L

let mod_inv a m =
  let (_, x, _) = ext_gcd a m in
  let r = Int64.rem x m in
  if r < 0L then Int64.add r m else r

let () =
  try
    let p = Scanf.scanf " %Ld" (fun x -> x) in
    let g = Scanf.scanf " %Ld" (fun x -> x) in
    let a = Scanf.scanf " %Ld" (fun x -> x) in
    let b = Scanf.scanf " %Ld" (fun x -> x) in
    let m = Scanf.scanf " %Ld" (fun x -> x) in
    let r = Scanf.scanf " %Ld" (fun x -> x) in
    let cap_a = mod_exp g a p in
    let cap_b = mod_exp g b p in
    let s = mod_exp cap_b a p in
    let c1 = mod_exp g r p in
    let c2 = Int64.rem (Int64.mul m (mod_exp cap_b r p)) p in
    let shared = mod_exp c1 b p in
    let inv_s = mod_inv shared p in
    let dm = Int64.rem (Int64.mul c2 inv_s) p in
    Printf.printf "%Ld %Ld %Ld\n" cap_a cap_b s;
    Printf.printf "%Ld %Ld\n" c1 c2;
    Printf.printf "%Ld\n" dm
  with End_of_file -> ()
