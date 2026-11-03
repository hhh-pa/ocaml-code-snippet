let () =
  try
    let t_cases = Scanf.scanf " %d" (fun x -> x) in
    for _ = 1 to t_cases do
      let tl = Scanf.scanf " %d" (fun x -> x) in
      let qn = Scanf.scanf " %f" (fun x -> x) in
      let qd = Scanf.scanf " %f" (fun x -> x) in
      let rn = Scanf.scanf " %f" (fun x -> x) in
      let rd = Scanf.scanf " %f" (fun x -> x) in
      let q = qn /. qd in
      let r = rn /. rd in

      let x0n = Scanf.scanf " %f" (fun x -> x) in
      let x0d = Scanf.scanf " %f" (fun x -> x) in
      let p0n = Scanf.scanf " %f" (fun x -> x) in
      let p0d = Scanf.scanf " %f" (fun x -> x) in
      let x = ref (x0n /. x0d) in
      let p = ref (p0n /. p0d) in

      let zs = Array.init tl (fun _ ->
        let n = Scanf.scanf " %f" (fun x -> x) in
        let d = Scanf.scanf " %f" (fun x -> x) in
        n /. d
      ) in

      Printf.printf "KALMAN\n";
      for k = 0 to tl - 1 do
        p := !p +. q;
        let k_gain = !p /. (!p +. r) in
        x := !x +. k_gain *. (zs.(k) -. !x);
        p := (1.0 -. k_gain) *. !p;
        Printf.printf "%.6f %.6f\n" !x !p
      done
    done
  with End_of_file -> ()
