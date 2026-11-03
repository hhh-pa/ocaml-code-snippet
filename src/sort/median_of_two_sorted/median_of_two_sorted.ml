let inf = 0x7fffffffffffffffL
let neg_inf = Int64.min_int

let rec find_median a b =
  let m = Array.length a in
  let n = Array.length b in
  if m > n then find_median b a
  else if m = 0 && n = 0 then 0.0
  else if m = 0 then begin
    let mid = n / 2 in
    if n mod 2 = 1 then Int64.to_float b.(mid)
    else (Int64.to_float b.(mid - 1) +. Int64.to_float b.(mid)) /. 2.0
  end else begin
    let lo = ref 0 in
    let hi = ref m in
    let total = m + n in
    let res = ref 0.0 in
    let found = ref false in
    while !lo <= !hi && not !found do
      let cut_a = (!lo + !hi) / 2 in
      let cut_b = (total + 1) / 2 - cut_a in
      let max_left_a = if cut_a = 0 then neg_inf else a.(cut_a - 1) in
      let min_right_a = if cut_a = m then inf else a.(cut_a) in
      let max_left_b = if cut_b = 0 then neg_inf else b.(cut_b - 1) in
      let min_right_b = if cut_b = n then inf else b.(cut_b) in
      if max_left_a <= min_right_b && max_left_b <= min_right_a then begin
        if total mod 2 = 1 then
          res := Int64.to_float (max max_left_a max_left_b)
        else
          res := (Int64.to_float (max max_left_a max_left_b) +.
                  Int64.to_float (min min_right_a min_right_b)) /. 2.0;
        found := true
      end else if max_left_a > min_right_b then
        hi := cut_a - 1
      else
        lo := cut_a + 1
    done;
    !res
  end

let () =
  try
    let m = Scanf.scanf " %d" (fun x -> x) in
    let n = Scanf.scanf " %d" (fun x -> x) in
    let a = Array.init m (fun _ -> Scanf.scanf " %Ld" (fun x -> x)) in
    let b = Array.init n (fun _ -> Scanf.scanf " %Ld" (fun x -> x)) in
    Array.sort compare a;
    Array.sort compare b;
    let ans = find_median a b in
    let iv = Int64.of_float ans in
    if ans = Int64.to_float iv then
      Printf.printf "%Ld\n" iv
    else
      Printf.printf "%g\n" ans
  with End_of_file -> ()
