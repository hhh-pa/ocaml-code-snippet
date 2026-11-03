let c1 = 0x9E3779B97F4A7C15L
let c2 = 0xBF58476D1CE4E5B9L

let add bits m k ux =
  let m64 = Int64.of_int m in
  let h1 = Int64.mul ux c1 in
  let h2 = Int64.logor (Int64.add (Int64.mul ux c2) 1L) 1L in
  for i = 0 to k - 1 do
    let term = Int64.add h1 (Int64.mul (Int64.of_int i) h2) in
    let pos = Int64.to_int (Int64.unsigned_rem term m64) in
    bits.(pos) <- 1
  done

let query bits m k ux =
  let m64 = Int64.of_int m in
  let h1 = Int64.mul ux c1 in
  let h2 = Int64.logor (Int64.add (Int64.mul ux c2) 1L) 1L in
  let ok = ref true in
  let i = ref 0 in
  while !ok && !i < k do
    let term = Int64.add h1 (Int64.mul (Int64.of_int !i) h2) in
    let pos = Int64.to_int (Int64.unsigned_rem term m64) in
    if bits.(pos) = 0 then ok := false;
    incr i
  done;
  !ok

let () =
  try
    let m = Scanf.scanf " %d" (fun x -> x) in
    let k = Scanf.scanf " %d" (fun x -> x) in
    let n = Scanf.scanf " %d" (fun x -> x) in
    let q = Scanf.scanf " %d" (fun x -> x) in
    let bits = Array.make m 0 in
    for _ = 1 to n do
      let x = Scanf.scanf " %Ld" (fun x -> x) in
      add bits m k x
    done;
    for _ = 1 to q do
      let x = Scanf.scanf " %Ld" (fun x -> x) in
      if query bits m k x then
        Printf.printf "YES\n"
      else
        Printf.printf "NO\n"
    done
  with End_of_file -> ()
