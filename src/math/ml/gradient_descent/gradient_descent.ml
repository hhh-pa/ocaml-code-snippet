let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let a = Array.make_matrix n n 0.0 in
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        a.(i).(j) <- Scanf.scanf " %f" (fun x -> x)
      done
    done;
    let b = Array.init n (fun _ -> Scanf.scanf " %f" (fun x -> x)) in
    let x = Array.init n (fun _ -> Scanf.scanf " %f" (fun x -> x)) in
    let lr = Scanf.scanf " %f" (fun x -> x) in
    let iters = Scanf.scanf " %d" (fun x -> x) in

    for _ = 1 to iters do
      let g = Array.make n 0.0 in
      for i = 0 to n - 1 do
        let s = ref 0.0 in
        for j = 0 to n - 1 do
          s := !s +. a.(i).(j) *. x.(j)
        done;
        g.(i) <- 2.0 *. !s +. b.(i)
      done;
      for i = 0 to n - 1 do
        x.(i) <- x.(i) -. lr *. g.(i)
      done
    done;

    let f = ref 0.0 in
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        f := !f +. x.(i) *. a.(i).(j) *. x.(j)
      done
    done;
    for i = 0 to n - 1 do
      f := !f +. b.(i) *. x.(i)
    done;

    for i = 0 to n - 1 do
      if i > 0 then print_string " ";
      let v = x.(i) +. 0.0 in
      let clean_v = if abs_float v < 1e-12 then 0.0 else v in
      Printf.printf "%.6f" clean_v
    done;
    print_newline ();
    let final_f = !f +. 0.0 in
    let clean_f = if abs_float final_f < 1e-12 then 0.0 else final_f in
    Printf.printf "%.6f\n" clean_f
  with End_of_file -> ()
