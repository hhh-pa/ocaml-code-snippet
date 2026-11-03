let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let m = Scanf.scanf " %d" (fun x -> x) in
    let es = Array.init m (fun _ ->
      let u = Scanf.scanf " %d" (fun x -> x) in
      let v = Scanf.scanf " %d" (fun x -> x) in
      let p = Scanf.scanf " %f" (fun x -> x) in
      let q = Scanf.scanf " %f" (fun x -> x) in
      let w = -. (log (p /. q)) in
      (u, v, w)
    ) in
    let d = Array.make n 0.0 in
    for _ = 0 to n - 1 do
      Array.iter (fun (u, v, w) ->
        if d.(u) +. w < d.(v) then
          d.(v) <- d.(u) +. w
      ) es
    done;
    let has_arbitrage = ref false in
    Array.iter (fun (u, v, w) ->
      if d.(u) +. w < d.(v) -. 1e-9 then
        has_arbitrage := true
      else if d.(u) +. w < d.(v) then
        has_arbitrage := true
    ) es;
    if !has_arbitrage then
      Printf.printf "ARBITRAGE\n"
    else
      Printf.printf "NO ARBITRAGE\n"
  with End_of_file -> ()
