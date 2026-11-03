let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let m = Scanf.scanf " %d" (fun x -> x) in
    let adj = Array.make n [] in
    for _ = 1 to m do
      let u = Scanf.scanf " %d" (fun x -> x) in
      let v = Scanf.scanf " %d" (fun x -> x) in
      let p = Scanf.scanf " %f" (fun x -> x) in
      adj.(u) <- (v, p) :: adj.(u)
    done;
    for u = 0 to n - 1 do
      adj.(u) <- List.rev adj.(u)
    done;
    let s = Scanf.scanf " %d" (fun x -> x) in
    let t = Scanf.scanf " %d" (fun x -> x) in
    let neg = -1e100 in
    let best = Array.make n neg in
    best.(s) <- 0.0;
    for u = 0 to n - 1 do
      if best.(u) <> neg then
        List.iter (fun (v, p) ->
          if p > 0.0 then
            let c = best.(u) +. log p in
            if c > best.(v) then best.(v) <- c
        ) adj.(u)
    done;
    if best.(t) = neg then
      Printf.printf "%.6f\n" (-1.0)
    else
      Printf.printf "%.6f\n" best.(t)
  with End_of_file -> ()
