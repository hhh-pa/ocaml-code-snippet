let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let p = Array.make (n + 1) 0.0 in
    for i = 1 to n do
      p.(i) <- Scanf.scanf " %f" (fun x -> x)
    done;
    let q = Array.make (n + 1) 0.0 in
    for i = 0 to n do
      q.(i) <- Scanf.scanf " %f" (fun x -> x)
    done;
    let e = Array.make_matrix (n + 2) (n + 2) 0.0 in
    let w = Array.make_matrix (n + 2) (n + 2) 0.0 in
    for i = 1 to n + 1 do
      e.(i).(i - 1) <- q.(i - 1);
      w.(i).(i - 1) <- q.(i - 1)
    done;
    for l = 1 to n do
      for i = 1 to n - l + 1 do
        let j = i + l - 1 in
        w.(i).(j) <- w.(i).(j - 1) +. p.(j) +. q.(j);
        e.(i).(j) <- 1e100;
        let wij = w.(i).(j) in
        for r = i to j do
          let c = e.(i).(r - 1) +. e.(r + 1).(j) +. wij in
          if c < e.(i).(j) then e.(i).(j) <- c
        done
      done
    done;
    Printf.printf "%.6f\n" e.(1).(n)
  with End_of_file -> ()
