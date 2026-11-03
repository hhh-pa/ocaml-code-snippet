let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let m = Scanf.scanf " %d" (fun x -> x) in
    let coins = Array.init m (fun _ -> Scanf.scanf " %d" (fun c -> c)) in
    let dp = Array.make (n + 1) 0L in
    dp.(0) <- 1L;
    for i = 0 to m - 1 do
      let c = coins.(i) in
      for j = c to n do
        dp.(j) <- Int64.add dp.(j) dp.(j - c)
      done
    done;
    Printf.printf "%Ld\n" dp.(n)
  with End_of_file -> ()
