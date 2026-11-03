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
  if m = 1L then 0L
  else
    let norm x =
      let r = Int64.rem x m in
      if r < 0L then Int64.add r m else r
    in
    let rec loop cur_b cur_e r =
      if cur_e = 0L then r
      else
        let new_r = if Int64.logand cur_e 1L = 1L then mod_mul r cur_b m else r in
        let new_b = mod_mul cur_b cur_b m in
        loop new_b (Int64.shift_right_logical cur_e 1) new_r
    in
    loop (norm b) e (Int64.rem 1L m)

let () =
  let tokens = ref [] in
  let rec read_all () =
    try
      let v = Scanf.scanf " %Ld" (fun x -> x) in
      tokens := v :: !tokens;
      read_all ()
    with End_of_file -> ()
  in
  read_all ();
  let vals = Array.of_list (List.rev !tokens) in
  let cnt = Array.length vals in
  if cnt = 0 then ()
  else if cnt = 3 then
    Printf.printf "%Ld\n" (modpow vals.(0) vals.(1) vals.(2))
  else
    let t = Int64.to_int vals.(0) in
    if t > 0 && cnt = 3 * t + 1 then
      for i = 0 to t - 1 do
        Printf.printf "%Ld\n" (modpow vals.(1 + 3 * i) vals.(1 + 3 * i + 1) vals.(1 + 3 * i + 2))
      done
    else
      Printf.printf "%Ld\n" (modpow vals.(0) vals.(1) vals.(2))
