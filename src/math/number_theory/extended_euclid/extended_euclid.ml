let rec extgcd a b =
  if b = 0L then
    let x = if a >= 0L then 1L else -1L in
    let g = if a >= 0L then a else Int64.neg a in
    (g, x, 0L)
  else
    let q = Int64.div a b in
    let r = Int64.rem a b in
    let (g, x1, y1) = extgcd b r in
    (g, y1, Int64.sub x1 (Int64.mul q y1))

let () =
  try
    let a = Scanf.scanf " %Ld" (fun x -> x) in
    let b = Scanf.scanf " %Ld" (fun x -> x) in
    if a = 0L && b = 0L then
      Printf.printf "0 0 0\n"
    else begin
      let aa = if a < 0L then Int64.neg a else a in
      let bb = if b < 0L then Int64.neg b else b in
      let (g, x, y) = extgcd aa bb in
      let final_x = if a < 0L then Int64.neg x else x in
      let final_y = if b < 0L then Int64.neg y else y in
      Printf.printf "%Ld %Ld %Ld\n" g final_x final_y
    end
  with End_of_file -> ()
