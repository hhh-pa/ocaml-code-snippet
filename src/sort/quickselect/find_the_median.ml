let partition arr l r =
  let pivot = arr.(r) in
  let i = ref l in
  for j = l to r - 1 do
    if arr.(j) <= pivot then begin
      let tmp = arr.(!i) in
      arr.(!i) <- arr.(j);
      arr.(j) <- tmp;
      incr i
    end
  done;
  let tmp = arr.(!i) in
  arr.(!i) <- arr.(r);
  arr.(r) <- tmp;
  !i

let rec quickselect arr l r k =
  if l <= r then begin
    let p = partition arr l r in
    if p = k then arr.(p)
    else if p > k then quickselect arr l (p - 1) k
    else quickselect arr (p + 1) r k
  end else
    -1

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let arr = Array.init n (fun _ -> Scanf.scanf " %d" (fun x -> x)) in
    let ans = quickselect arr 0 (n - 1) (n / 2) in
    Printf.printf "%d\n" ans
  with End_of_file -> ()
