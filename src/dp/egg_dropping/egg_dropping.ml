let () =
  try
    let k = Scanf.scanf " %Ld" (fun x -> x) in
    let n = Scanf.scanf " %Ld" (fun x -> x) in
    if n <= 0L then
      Printf.printf "0\n"
    else if k <= 0L then
      Printf.printf "-1\n"
    else if k = 1L then
      Printf.printf "%Ld\n" n
    else
      let ki = Int64.to_int k in
      let dp = Array.make (ki + 1) 0L in
      let m = ref 0L in
      let n_plus_1 = Int64.add n 1L in
      while dp.(ki) < n do
        m := Int64.add !m 1L;
        for e = ki downto 1 do
          let v = Int64.add (Int64.add dp.(e) dp.(e - 1)) 1L in
          dp.(e) <- (if v > n || v < 0L then n_plus_1 else v)
        done
      done;
      Printf.printf "%Ld\n" !m
  with End_of_file -> ()
