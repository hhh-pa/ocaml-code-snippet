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

let () =
  try
    let a = Scanf.scanf " %Ld" (fun x -> x) in
    let p = Scanf.scanf " %Ld" (fun x -> x) in

    let norm x =
      let r = Int64.rem x p in
      if r < 0L then Int64.add r p else r
    in
    let a = norm a in

    if a = 0L then
      Printf.printf "0\n"
    else if p = 2L then
      Printf.printf "%Ld\n" a
    else if mod_pow a (Int64.div (Int64.sub p 1L) 2L) p <> 1L then
      Printf.printf "NONE\n"
    else begin
      let q = ref (Int64.sub p 1L) in
      let s = ref 0 in
      while Int64.logand !q 1L = 0L do
        q := Int64.shift_right_logical !q 1;
        incr s
      done;

      if !s = 1 then begin
        let r = mod_pow a (Int64.div (Int64.add p 1L) 4L) p in
        let ans = min r (Int64.sub p r) in
        Printf.printf "%Ld\n" ans
      end else begin
        let z = ref 2L in
        let half_p = Int64.div (Int64.sub p 1L) 2L in
        let p_minus_1 = Int64.sub p 1L in
        while mod_pow !z half_p p <> p_minus_1 do
          z := Int64.add !z 1L
        done;

        let c = ref (mod_pow !z !q p) in
        let r = ref (mod_pow a (Int64.div (Int64.add !q 1L) 2L) p) in
        let t = ref (mod_pow a !q p) in
        let m = ref !s in

        while !t <> 1L do
          let t2 = ref !t in
          let i = ref 1 in
          let break_loop = ref false in
          while !i < !m && not !break_loop do
            t2 := mod_mul !t2 !t2 p;
            if !t2 = 1L then break_loop := true
            else incr i
          done;
          let shift_amt = !m - !i - 1 in
          let exp = Int64.shift_left 1L shift_amt in
          let b = mod_pow !c exp p in
          r := mod_mul !r b p;
          c := mod_mul b b p;
          t := mod_mul !t !c p;
          m := !i
        done;

        let ans = min !r (Int64.sub p !r) in
        Printf.printf "%Ld\n" ans
      end
    end
  with End_of_file -> ()
