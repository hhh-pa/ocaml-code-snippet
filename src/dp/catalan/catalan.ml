let cat k =
  let c = Array.make (k + 1) 0L in
  c.(0) <- 1L;
  for i = 1 to k do
    let s = ref 0L in
    for j = 0 to i - 1 do
      s := Int64.add !s (Int64.mul c.(j) c.(i - 1 - j))
    done;
    c.(i) <- !s
  done;
  c.(k)

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let m = Scanf.scanf " %d" (fun x -> x) in
    Printf.printf "%Ld\n" (cat n);
    Printf.printf "%Ld\n" (if m >= 2 then cat (m - 2) else 0L)
  with End_of_file -> ()
