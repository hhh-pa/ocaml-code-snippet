let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let x = Array.make n 0.0 in
    let y = Array.make n 0.0 in
    for i = 0 to n - 1 do
      x.(i) <- Scanf.scanf " %f" (fun v -> v);
      y.(i) <- Scanf.scanf " %f" (fun v -> v)
    done;
    if n <= 1 then
      Printf.printf "%.6f\n" 0.0
    else if n = 2 then
      let d = hypot (x.(1) -. x.(0)) (y.(1) -. y.(0)) in
      Printf.printf "%.6f\n" (2.0 *. d)
    else
      let dist a b =
        hypot (x.(a) -. x.(b)) (y.(a) -. y.(b))
      in
      let dp = Array.make_matrix n n 1e100 in
      dp.(0).(1) <- dist 0 1;
      for j = 2 to n - 1 do
        for i = 0 to j - 2 do
          dp.(i).(j) <- dp.(i).(j - 1) +. dist (j - 1) j
        done;
        let best = ref 1e100 in
        for k = 0 to j - 2 do
          let v = dp.(k).(j - 1) +. dist k j in
          if v < !best then best := v
        done;
        dp.(j - 1).(j) <- !best
      done;
      let ans = dp.(n - 2).(n - 1) +. dist (n - 2) (n - 1) in
      Printf.printf "%.6f\n" ans
  with End_of_file -> ()
