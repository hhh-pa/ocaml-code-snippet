let rec gcdll a b =
  let a = if a < 0L then Int64.neg a else a in
  let b = if b < 0L then Int64.neg b else b in
  let rec loop x y =
    if y = 0L then (if x = 0L then 1L else x)
    else loop y (Int64.rem x y)
  in
  loop a b

let norm p q =
  let (p, q) = if q < 0L then (Int64.neg p, Int64.neg q) else (p, q) in
  let g = gcdll p q in
  (Int64.div p g, Int64.div q g)

let cmpf (p1, q1) (p2, q2) =
  let l = Int64.mul p1 q2 in
  let r = Int64.mul p2 q1 in
  Int64.compare l r

let solve_case () =
  let d = Scanf.scanf " %d" (fun x -> x) in
  let types = Scanf.scanf " %s" (fun s -> s) in
  let n = 1 lsl d in
  let leaves = Array.make n 0L in
  for i = 0 to n - 1 do
    leaves.(i) <- Scanf.scanf " %Ld" (fun x -> x)
  done;

  let rec solve idx level =
    if level = String.length types then
      norm leaves.(idx) 1L
    else
      let a = solve (idx * 2) (level + 1) in
      let b = solve (idx * 2 + 1) (level + 1) in
      let t = types.[level] in
      if t = 'X' then
        if cmpf a b >= 0 then a else b
      else if t = 'N' then
        if cmpf a b <= 0 then a else b
      else
        let (p1, q1) = a in
        let (p2, q2) = b in
        let num = Int64.add (Int64.mul p1 q2) (Int64.mul p2 q1) in
        let den = Int64.mul 2L (Int64.mul q1 q2) in
        norm num den
  in
  let (rp, rq) = solve 0 0 in
  Printf.printf "EXPECTI %Ld/%Ld\n" rp rq

let () =
  try
    let t = Scanf.scanf " %d" (fun x -> x) in
    for _ = 0 to t - 1 do
      solve_case ()
    done
  with End_of_file -> ()
