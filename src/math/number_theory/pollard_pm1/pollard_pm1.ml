let mod_mul a b m =
  if m <= 3_000_000_000L then
    Int64.rem (Int64.mul a b) m
  else
    let rec loop cur_a cur_b acc =
      if cur_b = 0L then acc
      else
        let new_acc =
          if Int64.logand cur_b 1L = 1L then
            let sum = Int64.add acc cur_a in
            if sum >= m || sum < 0L then Int64.rem sum m else sum
          else acc
        in
        let double_a = Int64.add cur_a cur_a in
        let new_a =
          if double_a >= m || double_a < 0L then Int64.rem double_a m else double_a
        in
        loop new_a (Int64.shift_right_logical cur_b 1) new_acc
    in
    loop (Int64.rem a m) b 0L

let mod_pow base exp m =
  let rec loop b e acc =
    if e = 0L then acc
    else
      let new_acc = if Int64.logand e 1L = 1L then mod_mul acc b m else acc in
      let new_b = mod_mul b b m in
      loop new_b (Int64.shift_right_logical e 1) new_acc
  in
  loop (Int64.rem base m) exp (Int64.rem 1L m)

let rec llgcd a b =
  if b = 0L then (if a < 0L then Int64.neg a else a)
  else llgcd b (Int64.rem a b)

let () =
  try
    let n = Scanf.scanf " %Ld" (fun x -> x) in
    let b_bound = Scanf.scanf " %Ld" (fun x -> x) in

    if n <= 1L then begin
      Printf.printf "NONE\n";
      exit 0
    end;

    let a = ref 2L in
    let found = ref false in
    let k = ref 2L in
    while !k <= b_bound && not !found do
      a := mod_pow !a !k n;
      let diff = if !a > 0L then Int64.add (Int64.sub !a 1L) n else Int64.sub n 1L in
      let g = llgcd diff n in
      if g > 1L && g < n then begin
        Printf.printf "%Ld\n" g;
        found := true
      end;
      k := Int64.add !k 1L
    done;

    if not !found then begin
      let diff2 = Int64.rem (Int64.add (Int64.sub !a 1L) n) n in
      let g = llgcd diff2 n in
      if g > 1L && g < n then
        Printf.printf "%Ld\n" g
      else
        Printf.printf "NONE\n"
    end
  with End_of_file -> ()
