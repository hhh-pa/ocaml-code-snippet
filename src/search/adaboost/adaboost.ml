type stump = {
  f : int;
  t : int;
  pol : int;
  alpha : float;
}

let rec uniq = function
  | [] -> []
  | [x] -> [x]
  | x :: (y :: _ as tl) -> if x = y then uniq tl else x :: uniq tl

let solve () =
  let n = Scanf.scanf " %d" (fun x -> x) in
  let f_cnt = Scanf.scanf " %d" (fun x -> x) in
  let r_cnt = Scanf.scanf " %d" (fun x -> x) in
  let x_mat = Array.make_matrix n f_cnt 0 in
  let y_arr = Array.make n 0 in
  for i = 0 to n - 1 do
    for f = 0 to f_cnt - 1 do
      x_mat.(i).(f) <- Scanf.scanf " %d" (fun x -> x)
    done;
    y_arr.(i) <- Scanf.scanf " %d" (fun x -> x)
  done;
  let w = Array.make n (1.0 /. float_of_int n) in
  let ens = ref [] in
  let stop = ref false in
  let rd = ref 0 in
  while !rd < r_cnt && not !stop do
    let bf = ref 0 in
    let bt = ref 0 in
    let bpol = ref 0 in
    let be = ref 1e300 in
    for f = 0 to f_cnt - 1 do
      let vals = ref [] in
      for i = 0 to n - 1 do
        vals := x_mat.(i).(f) :: !vals
      done;
      let uvals = uniq (List.sort compare !vals) in
      List.iter (fun t_val ->
        for pol = 0 to 1 do
          let e = ref 0.0 in
          for i = 0 to n - 1 do
            let h = if (x_mat.(i).(f) >= t_val) = (pol = 1) then 1 else 0 in
            if h <> y_arr.(i) then e := !e +. w.(i)
          done;
          if !e < !be -. 1e-12 then begin
            be := !e;
            bf := f;
            bt := t_val;
            bpol := pol
          end
        done
      ) uvals
    done;
    if !be >= 0.5 then
      stop := true
    else begin
      let alpha = if !be <= 0.0 then 10.0 else 0.5 *. log ((1.0 -. !be) /. !be) in
      let tot = ref 0.0 in
      for i = 0 to n - 1 do
        let h = if (x_mat.(i).(!bf) >= !bt) = (!bpol = 1) then 1.0 else -1.0 in
        let y = if y_arr.(i) = 1 then 1.0 else -1.0 in
        w.(i) <- w.(i) *. exp (-. alpha *. y *. h);
        tot := !tot +. w.(i)
      done;
      for i = 0 to n - 1 do
        w.(i) <- w.(i) /. !tot
      done;
      ens := { f = !bf; t = !bt; pol = !bpol; alpha } :: !ens;
      if !be <= 0.0 then stop := true
    end;
    incr rd
  done;
  let stumps = List.rev !ens in
  let p_arr = Array.make n 0 in
  let err = ref 0 in
  for i = 0 to n - 1 do
    let s = ref 0.0 in
    List.iter (fun st ->
      let h = if (x_mat.(i).(st.f) >= st.t) = (st.pol = 1) then 1.0 else -1.0 in
      s := !s +. st.alpha *. h
    ) stumps;
    let pred = if !s >= 0.0 then 1 else 0 in
    p_arr.(i) <- pred;
    if pred <> y_arr.(i) then incr err
  done;
  Printf.printf "ADA %d %d\n" (List.length stumps) !err;
  for i = 0 to n - 1 do
    if i > 0 then Printf.printf " ";
    Printf.printf "%d" p_arr.(i)
  done;
  Printf.printf "\n"

let () =
  try
    let t = Scanf.scanf " %d" (fun x -> x) in
    for _ = 0 to t - 1 do
      solve ()
    done
  with End_of_file -> ()
