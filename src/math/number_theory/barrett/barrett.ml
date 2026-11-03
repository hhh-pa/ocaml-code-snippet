let () =
  try
    let x = Scanf.scanf " %Ld" (fun v -> v) in
    let m = Scanf.scanf " %Ld" (fun v -> v) in
    let one_shift_32 = Int64.shift_left 1L 32 in
    let mu = Int64.div one_shift_32 m in
    let q = ref (Int64.shift_right_logical (Int64.mul x mu) 32) in
    let r = ref (Int64.sub x (Int64.mul !q m)) in
    while !r < 0L do
      r := Int64.add !r m;
      q := Int64.sub !q 1L
    done;
    while !r >= m do
      r := Int64.sub !r m;
      q := Int64.add !q 1L
    done;
    Printf.printf "%Ld %Ld\n" !q !r
  with End_of_file -> ()
