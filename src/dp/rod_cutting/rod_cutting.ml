let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let p = Array.init n (fun _ -> Scanf.scanf " %d" (fun x -> x)) in
    let dp = Array.make (n + 1) 0 in
    for i = 1 to n do
      let b = ref min_int in
      for j = 1 to i do
        if j <= n then
          let v = p.(j - 1) + dp.(i - j) in
          if v > !b then b := v
      done;
      dp.(i) <- (if !b = min_int then 0 else !b)
    done;
    Printf.printf "%d\n" dp.(n)
  with End_of_file -> ()
