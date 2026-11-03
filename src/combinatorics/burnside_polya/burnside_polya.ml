let phi n =
  let r = ref n in
  let x = ref n in
  let p = ref 2L in
  while Int64.mul !p !p <= !x do
    if Int64.rem !x !p = 0L then begin
      while Int64.rem !x !p = 0L do
        x := Int64.div !x !p
      done;
      r := Int64.sub !r (Int64.div !r !p)
    end;
    p := Int64.succ !p
  done;
  if !x > 1L then r := Int64.sub !r (Int64.div !r !x);
  !r

let ipow b e =
  let r = ref 1L in
  let base = ref b in
  let exp = ref e in
  while !exp > 0L do
    if Int64.logand !exp 1L = 1L then r := Int64.mul !r !base;
    base := Int64.mul !base !base;
    exp := Int64.shift_right_logical !exp 1
  done;
  !r

let necklaces n k =
  let s = ref 0L in
  let d = ref 1L in
  while !d <= n do
    if Int64.rem n !d = 0L then
      s := Int64.add !s (Int64.mul (phi !d) (ipow k (Int64.div n !d)));
    d := Int64.succ !d
  done;
  Int64.div !s n

let () =
  try
    let mode, n, k = Scanf.scanf " %s %Ld %Ld" (fun m n k -> (m, n, k)) in
    let cap_n = necklaces n k in
    if mode = "N" then
      Printf.printf "%Ld\n" cap_n
    else if Int64.rem n 2L = 1L then
      let ans = Int64.div (Int64.add cap_n (ipow k (Int64.div (Int64.add n 1L) 2L))) 2L in
      Printf.printf "%Ld\n" ans
    else
      let term1 = Int64.mul 2L cap_n in
      let term2 = ipow k (Int64.add (Int64.div n 2L) 1L) in
      let term3 = ipow k (Int64.div n 2L) in
      let ans = Int64.div (Int64.add (Int64.add term1 term2) term3) 4L in
      Printf.printf "%Ld\n" ans
  with End_of_file -> ()
