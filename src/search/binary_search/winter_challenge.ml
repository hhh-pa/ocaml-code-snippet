let bisect_left a key n =
  let low = ref 0 in
  let high = ref n in
  while !low < !high do
    let mid = (!low + !high) / 2 in
    if a.(mid) < key then low := mid + 1
    else high := mid
  done;
  !low

let bisect_right a key n =
  let low = ref 0 in
  let high = ref n in
  while !low < !high do
    let mid = (!low + !high) / 2 in
    if a.(mid) <= key then low := mid + 1
    else high := mid
  done;
  !low

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let a = Array.make n 0 in
    for i = 0 to n - 1 do
      a.(i) <- Scanf.scanf " %d" (fun x -> x)
    done;
    let key = Scanf.scanf " %d" (fun x -> x) in
    let lb = bisect_left a key n in
    let ub = bisect_right a key n in
    if lb >= n || a.(lb) <> key then
      Printf.printf "-1 -1 0\n"
    else
      Printf.printf "%d %d %d\n" lb (ub - 1) (ub - lb)
  with End_of_file -> ()
