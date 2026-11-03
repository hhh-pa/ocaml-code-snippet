type tr = { s2 : int; p : float; r : float }

let solve_case () =
  let s_count = Scanf.scanf " %d" (fun x -> x) in
  let a_count = Scanf.scanf " %d" (fun x -> x) in
  let tr = Array.make_matrix s_count a_count [] in
  for s = 0 to s_count - 1 do
    for a = 0 to a_count - 1 do
      let nt = Scanf.scanf " %d" (fun x -> x) in
      let trans = ref [] in
      for _ = 0 to nt - 1 do
        let s2 = Scanf.scanf " %d" (fun x -> x) in
        let n = Scanf.scanf " %f" (fun x -> x) in
        let d = Scanf.scanf " %f" (fun x -> x) in
        let r = Scanf.scanf " %f" (fun x -> x) in
        trans := { s2; p = n /. d; r } :: !trans
      done;
      tr.(s).(a) <- List.rev !trans
    done
  done;
  let gn = Scanf.scanf " %f" (fun x -> x) in
  let gd = Scanf.scanf " %f" (fun x -> x) in
  let g = gn /. gd in
  let k = Scanf.scanf " %d" (fun x -> x) in
  let u = Array.make s_count 0.0 in
  let u2 = Array.make s_count 0.0 in
  let pol = Array.make s_count 0 in

  let round = ref 0 in
  let stop = ref false in
  while !round < 100 && not !stop do
    incr round;
    for _ = 0 to k - 1 do
      for s = 0 to s_count - 1 do
        let v = ref 0.0 in
        List.iter (fun t ->
          v := !v +. t.p *. (t.r +. g *. u.(t.s2))
        ) tr.(s).(pol.(s));
        u2.(s) <- !v
      done;
      Array.blit u2 0 u 0 s_count
    done;

    let stable = ref true in
    for s = 0 to s_count - 1 do
      let best = ref 0.0 in
      let ba = ref pol.(s) in
      List.iter (fun t ->
        best := !best +. t.p *. (t.r +. g *. u.(t.s2))
      ) tr.(s).(pol.(s));
      for a = 0 to a_count - 1 do
        let v = ref 0.0 in
        List.iter (fun t ->
          v := !v +. t.p *. (t.r +. g *. u.(t.s2))
        ) tr.(s).(a);
        if !v > !best +. 1e-12 then begin
          best := !v;
          ba := a
        end
      done;
      if !ba <> pol.(s) then begin
        pol.(s) <- !ba;
        stable := false
      end
    done;
    if !stable then stop := true
  done;

  Printf.printf "PI\n";
  for s = 0 to s_count - 1 do
    if s > 0 then print_string " ";
    Printf.printf "%.6f" u.(s)
  done;
  print_newline ();
  for s = 0 to s_count - 1 do
    if s > 0 then print_string " ";
    print_int pol.(s)
  done;
  print_newline ()

let () =
  try
    let t = Scanf.scanf " %d" (fun x -> x) in
    for _ = 0 to t - 1 do
      solve_case ()
    done
  with End_of_file -> ()
