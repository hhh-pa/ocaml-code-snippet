let solve () =
  let n = Scanf.scanf " %d" (fun x -> x) in
  let f_cnt = Scanf.scanf " %d" (fun x -> x) in
  let h_cnt = Scanf.scanf " %d" (fun x -> x) in
  let e_cnt = Scanf.scanf " %d" (fun x -> x) in
  let x_mat = Array.make_matrix n f_cnt 0 in
  let y_arr = Array.make n 0 in
  for i = 0 to n - 1 do
    for f = 0 to f_cnt - 1 do
      x_mat.(i).(f) <- Scanf.scanf " %d" (fun x -> x)
    done;
    y_arr.(i) <- Scanf.scanf " %d" (fun x -> x)
  done;

  Printf.printf "BACKPROP\n";

  (* Perceptron *)
  let w = Array.make f_cnt 0 in
  let b = ref 0 in
  for _ = 0 to e_cnt - 1 do
    for i = 0 to n - 1 do
      let y = if y_arr.(i) = 1 then 1 else -1 in
      let act = ref !b in
      for f = 0 to f_cnt - 1 do
        act := !act + w.(f) * x_mat.(i).(f)
      done;
      if y * !act <= 0 then begin
        for f = 0 to f_cnt - 1 do
          w.(f) <- w.(f) + y * x_mat.(i).(f)
        done;
        b := !b + y
      end
    done
  done;
  Printf.printf "P";
  for f = 0 to f_cnt - 1 do
    Printf.printf " %d" w.(f)
  done;
  Printf.printf " %d" !b;
  for i = 0 to n - 1 do
    let act = ref !b in
    for f = 0 to f_cnt - 1 do
      act := !act + w.(f) * x_mat.(i).(f)
    done;
    Printf.printf " %d" (if !act > 0 then 1 else 0)
  done;
  Printf.printf "\n";

  (* Logistic regression *)
  let wl = Array.make f_cnt 0.0 in
  let bl = ref 0.0 in
  for _ = 0 to e_cnt - 1 do
    for i = 0 to n - 1 do
      let z = ref !bl in
      for f = 0 to f_cnt - 1 do
        z := !z +. wl.(f) *. float_of_int x_mat.(i).(f)
      done;
      let p = 1.0 /. (1.0 +. exp (-. !z)) in
      let err = float_of_int y_arr.(i) -. p in
      for f = 0 to f_cnt - 1 do
        wl.(f) <- wl.(f) +. 0.5 *. err *. float_of_int x_mat.(i).(f)
      done;
      bl := !bl +. 0.5 *. err
    done
  done;
  Printf.printf "L";
  for f = 0 to f_cnt - 1 do
    Printf.printf " %.6f" wl.(f)
  done;
  Printf.printf " %.6f" !bl;
  for i = 0 to n - 1 do
    let z = ref !bl in
    for f = 0 to f_cnt - 1 do
      z := !z +. wl.(f) *. float_of_int x_mat.(i).(f)
    done;
    let p = 1.0 /. (1.0 +. exp (-. !z)) in
    Printf.printf " %d" (if p >= 0.5 then 1 else 0)
  done;
  Printf.printf "\n";

  (* MLP backprop *)
  let w1 = Array.make_matrix h_cnt f_cnt 0.0 in
  let b1 = Array.make h_cnt 0.0 in
  let w2 = Array.make h_cnt 0.0 in
  let b2 = ref 0.0 in
  for h = 0 to h_cnt - 1 do
    for f = 0 to f_cnt - 1 do
      w1.(h).(f) <- 0.05 *. float_of_int (h + f + 1)
    done;
    w2.(h) <- 0.05 *. float_of_int (h + 1)
  done;

  for _ = 0 to e_cnt - 1 do
    for i = 0 to n - 1 do
      let a1 = Array.make h_cnt 0.0 in
      for h = 0 to h_cnt - 1 do
        let z = ref b1.(h) in
        for f = 0 to f_cnt - 1 do
          z := !z +. w1.(h).(f) *. float_of_int x_mat.(i).(f)
        done;
        a1.(h) <- tanh !z
      done;
      let z2 = ref !b2 in
      for h = 0 to h_cnt - 1 do
        z2 := !z2 +. w2.(h) *. a1.(h)
      done;
      let p = 1.0 /. (1.0 +. exp (-. !z2)) in
      let d2 = (p -. float_of_int y_arr.(i)) *. p *. (1.0 -. p) in
      for h = 0 to h_cnt - 1 do
        let d1 = d2 *. w2.(h) *. (1.0 -. a1.(h) *. a1.(h)) in
        for f = 0 to f_cnt - 1 do
          w1.(h).(f) <- w1.(h).(f) -. 0.1 *. d1 *. float_of_int x_mat.(i).(f)
        done;
        b1.(h) <- b1.(h) -. 0.1 *. d1
      done;
      for h = 0 to h_cnt - 1 do
        w2.(h) <- w2.(h) -. 0.1 *. d2 *. a1.(h)
      done;
      b2 := !b2 -. 0.1 *. d2
    done
  done;

  Printf.printf "M";
  let loss = ref 0.0 in
  for i = 0 to n - 1 do
    let a1 = Array.make h_cnt 0.0 in
    for h = 0 to h_cnt - 1 do
      let z = ref b1.(h) in
      for f = 0 to f_cnt - 1 do
        z := !z +. w1.(h).(f) *. float_of_int x_mat.(i).(f)
      done;
      a1.(h) <- tanh !z
    done;
    let z2 = ref !b2 in
    for h = 0 to h_cnt - 1 do
      z2 := !z2 +. w2.(h) *. a1.(h)
    done;
    let p = 1.0 /. (1.0 +. exp (-. !z2)) in
    let diff = float_of_int y_arr.(i) -. p in
    loss := !loss +. 0.5 *. diff *. diff;
    Printf.printf " %d" (if p >= 0.5 then 1 else 0)
  done;
  loss := !loss /. float_of_int n;
  Printf.printf " %.6f\n" !loss

let () =
  try
    let t = Scanf.scanf " %d" (fun x -> x) in
    for _ = 0 to t - 1 do
      solve ()
    done
  with End_of_file -> ()
