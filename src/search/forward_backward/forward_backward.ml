let solve_case () =
  let s_states = Scanf.scanf " %d" (fun x -> x) in
  let o_obs = Scanf.scanf " %d" (fun x -> x) in
  let tl = Scanf.scanf " %d" (fun x -> x) in

  let pi = Array.make s_states 0.0 in
  for i = 0 to s_states - 1 do
    let n = Scanf.scanf " %f" (fun x -> x) in
    let d = Scanf.scanf " %f" (fun x -> x) in
    pi.(i) <- n /. d
  done;

  let a = Array.make_matrix s_states s_states 0.0 in
  for i = 0 to s_states - 1 do
    for j = 0 to s_states - 1 do
      let n = Scanf.scanf " %f" (fun x -> x) in
      let d = Scanf.scanf " %f" (fun x -> x) in
      a.(i).(j) <- n /. d
    done
  done;

  let b = Array.make_matrix s_states o_obs 0.0 in
  for i = 0 to s_states - 1 do
    for j = 0 to o_obs - 1 do
      let n = Scanf.scanf " %f" (fun x -> x) in
      let d = Scanf.scanf " %f" (fun x -> x) in
      b.(i).(j) <- n /. d
    done
  done;

  let obs = Array.make tl 0 in
  for k = 0 to tl - 1 do
    obs.(k) <- Scanf.scanf " %d" (fun x -> x)
  done;

  let al = Array.make_matrix tl s_states 0.0 in
  let be = Array.make_matrix tl s_states 0.0 in

  for i = 0 to s_states - 1 do
    al.(0).(i) <- pi.(i) *. b.(i).(obs.(0))
  done;

  for k = 1 to tl - 1 do
    let ob = obs.(k) in
    for j = 0 to s_states - 1 do
      let s = ref 0.0 in
      for i = 0 to s_states - 1 do
        s := !s +. al.(k - 1).(i) *. a.(i).(j)
      done;
      al.(k).(j) <- !s *. b.(j).(ob)
    done
  done;

  for i = 0 to s_states - 1 do
    be.(tl - 1).(i) <- 1.0
  done;

  for k = tl - 2 downto 0 do
    let next_ob = obs.(k + 1) in
    for i = 0 to s_states - 1 do
      let s = ref 0.0 in
      for j = 0 to s_states - 1 do
        s := !s +. a.(i).(j) *. b.(j).(next_ob) *. be.(k + 1).(j)
      done;
      be.(k).(i) <- !s
    done
  done;

  Printf.printf "FB\n";
  for k = 0 to tl - 1 do
    let tot = ref 0.0 in
    for i = 0 to s_states - 1 do
      tot := !tot +. al.(k).(i) *. be.(k).(i)
    done;
    for i = 0 to s_states - 1 do
      if i > 0 then Printf.printf " ";
      let g = if !tot = 0.0 then 0.0 else al.(k).(i) *. be.(k).(i) /. !tot in
      Printf.printf "%.6f" g
    done;
    Printf.printf "\n"
  done

let () =
  try
    let t = Scanf.scanf " %d" (fun x -> x) in
    for _ = 0 to t - 1 do
      solve_case ()
    done
  with End_of_file -> ()
