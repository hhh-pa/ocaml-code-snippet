let solve_case () =
  let n = Scanf.scanf " %d" (fun x -> x) in
  let it_cnt = Scanf.scanf " %d" (fun x -> x) in
  let x = Array.make n 0.0 in
  for i = 0 to n - 1 do
    let num = Scanf.scanf " %f" (fun v -> v) in
    let den = Scanf.scanf " %f" (fun v -> v) in
    x.(i) <- num /. den
  done;

  let mn = ref x.(0) in
  let mx = ref x.(0) in
  let sum_x = ref 0.0 in
  for i = 0 to n - 1 do
    if x.(i) < !mn then mn := x.(i);
    if x.(i) > !mx then mx := x.(i);
    sum_x := !sum_x +. x.(i)
  done;
  let mean = !sum_x /. float_of_int n in
  let sum_va = ref 0.0 in
  for i = 0 to n - 1 do
    let diff = x.(i) -. mean in
    sum_va := !sum_va +. diff *. diff
  done;
  let va = ref (!sum_va /. float_of_int n) in
  if !va < 1e-6 then va := 1e-6;

  let w0 = ref 0.5 in
  let w1 = ref 0.5 in
  let mu0 = ref !mn in
  let mu1 = ref !mx in
  let v0 = ref !va in
  let v1 = ref !va in
  let g0 = Array.make n 0.0 in
  let g1 = Array.make n 0.0 in

  for _ = 0 to it_cnt - 1 do
    let sqrt_v0 = sqrt !v0 in
    let sqrt_v1 = sqrt !v1 in
    for i = 0 to n - 1 do
      let d0 = x.(i) -. !mu0 in
      let d1 = x.(i) -. !mu1 in
      let p0 = !w0 *. exp (-0.5 *. d0 *. d0 /. !v0) /. sqrt_v0 in
      let p1 = !w1 *. exp (-0.5 *. d1 *. d1 /. !v1) /. sqrt_v1 in
      let s = p0 +. p1 in
      if s = 0.0 then begin
        g0.(i) <- 0.5;
        g1.(i) <- 0.5
      end else begin
        g0.(i) <- p0 /. s;
        g1.(i) <- p1 /. s
      end
    done;

    let n0 = ref 0.0 in
    let n1 = ref 0.0 in
    for i = 0 to n - 1 do
      n0 := !n0 +. g0.(i);
      n1 := !n1 +. g1.(i)
    done;
    w0 := !n0 /. float_of_int n;
    w1 := !n1 /. float_of_int n;

    let s_mu0 = ref 0.0 in
    let s_mu1 = ref 0.0 in
    for i = 0 to n - 1 do
      s_mu0 := !s_mu0 +. g0.(i) *. x.(i);
      s_mu1 := !s_mu1 +. g1.(i) *. x.(i)
    done;
    mu0 := !s_mu0 /. !n0;
    mu1 := !s_mu1 /. !n1;

    let s_v0 = ref 0.0 in
    let s_v1 = ref 0.0 in
    for i = 0 to n - 1 do
      let d0 = x.(i) -. !mu0 in
      let d1 = x.(i) -. !mu1 in
      s_v0 := !s_v0 +. g0.(i) *. d0 *. d0;
      s_v1 := !s_v1 +. g1.(i) *. d1 *. d1
    done;
    v0 := !s_v0 /. !n0;
    v1 := !s_v1 /. !n1;
    if !v0 < 1e-6 then v0 := 1e-6;
    if !v1 < 1e-6 then v1 := 1e-6
  done;

  if !mu1 < !mu0 then begin
    let tw = !w0 in w0 := !w1; w1 := tw;
    let tm = !mu0 in mu0 := !mu1; mu1 := tm;
    let tv = !v0 in v0 := !v1; v1 := tv;
    for i = 0 to n - 1 do
      let tg = g0.(i) in g0.(i) <- g1.(i); g1.(i) <- tg
    done
  end;

  Printf.printf "EM\n";
  Printf.printf "%.6f %.6f %.6f %.6f %.6f %.6f\n" !w0 !mu0 !v0 !w1 !mu1 !v1;
  for i = 0 to n - 1 do
    if i > 0 then Printf.printf " ";
    Printf.printf "%d" (if g1.(i) > g0.(i) then 1 else 0)
  done;
  Printf.printf "\n"

let () =
  try
    let t = Scanf.scanf " %d" (fun x -> x) in
    for _ = 0 to t - 1 do
      solve_case ()
    done
  with End_of_file -> ()
