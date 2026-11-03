let rec llgcd a b =
  let a = if a < 0L then Int64.neg a else a in
  let b = if b < 0L then Int64.neg b else b in
  if b = 0L then a else llgcd b (Int64.rem a b)

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

let modpow b e m =
  let rec loop cur_b cur_e r =
    if cur_e = 0L then r
    else
      let new_r = if Int64.logand cur_e 1L = 1L then mod_mul r cur_b m else r in
      let new_b = mod_mul cur_b cur_b m in
      loop new_b (Int64.shift_right_logical cur_e 1) new_r
  in
  loop (Int64.rem b m) e (Int64.rem 1L m)

let jacobi a n =
  let a_val = ref (Int64.rem a n) in
  let n_val = ref n in
  let t_val = ref 1 in
  while !a_val <> 0L do
    while Int64.rem !a_val 2L = 0L do
      a_val := Int64.shift_right_logical !a_val 1;
      let r = Int64.rem !n_val 8L in
      if r = 3L || r = 5L then t_val := - !t_val
    done;
    let tmp = !a_val in
    a_val := !n_val;
    n_val := tmp;
    if Int64.rem !a_val 4L = 3L && Int64.rem !n_val 4L = 3L then
      t_val := - !t_val;
    a_val := Int64.rem !a_val !n_val
  done;
  if !n_val = 1L then !t_val else 0

let () =
  try
    let n = Scanf.scanf " %Ld" (fun x -> x) in
    let t = Scanf.scanf " %d" (fun x -> x) in
    let b = Array.init t (fun _ -> Scanf.scanf " %Ld" (fun x -> x)) in
    let js = Array.make t 0 in
    let comp = ref false in

    if n < 2L then
      comp := true
    else if n = 2L then
      for i = 0 to t - 1 do
        js.(i) <- jacobi b.(i) n
      done
    else if Int64.rem n 2L = 0L then
      comp := true
    else begin
      for i = 0 to t - 1 do
        js.(i) <- jacobi b.(i) n;
        if llgcd b.(i) n > 1L then
          comp := true
        else begin
          let j_rem = Int64.rem (Int64.of_int js.(i)) n in
          let expected = Int64.rem (Int64.add j_rem n) n in
          let half = Int64.div (Int64.sub n 1L) 2L in
          let actual = modpow b.(i) half n in
          if js.(i) = 0 || actual <> expected then
            comp := true
        end
      done
    end;

    for i = 0 to t - 1 do
      if i > 0 then print_string " ";
      print_int js.(i)
    done;
    print_newline ();
    Printf.printf "%s\n" (if !comp then "COMPOSITE" else "PRIME")
  with End_of_file -> ()
