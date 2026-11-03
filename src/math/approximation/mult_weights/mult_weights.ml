let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let t_rounds = Scanf.scanf " %d" (fun x -> x) in
    let eta = Scanf.scanf " %f" (fun x -> x) in
    let w = Array.make n 1.0 in
    for _ = 1 to t_rounds do
      for i = 0 to n - 1 do
        let g = Scanf.scanf " %f" (fun x -> x) in
        w.(i) <- w.(i) *. (1.0 +. eta *. g)
      done
    done;
    let s = ref 0.0 in
    for i = 0 to n - 1 do
      s := !s +. w.(i)
    done;
    for i = 0 to n - 1 do
      if i > 0 then print_string " ";
      let v = (w.(i) /. !s) +. 0.0 in
      Printf.printf "%.6f" v
    done;
    print_newline ()
  with End_of_file -> ()
