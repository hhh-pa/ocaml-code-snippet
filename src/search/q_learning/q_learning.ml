let mult = 6364136223846793005L
let add = 1442695040888963407L

type lcg = { mutable s : int64 }

let make_lcg seed = { s = seed }

let next_lcg rng =
  rng.s <- Int64.add (Int64.mul rng.s mult) add;
  rng.s

let next_int rng n =
  let v = next_lcg rng in
  Int64.to_int (Int64.unsigned_rem v (Int64.of_int n))

let next_double rng =
  let v = next_lcg rng in
  let shifted = Int64.shift_right_logical v 11 in
  Int64.to_float shifted *. 1.1102230246251565e-16

type tr = { s2 : int; p : float; r : float }

let solve_case () =
  let s_count = Scanf.scanf " %d" (fun x -> x) in
  let a_count = Scanf.scanf " %d" (fun x -> x) in
  let tr = Array.make_matrix s_count a_count [||] in
  for s = 0 to s_count - 1 do
    for a = 0 to a_count - 1 do
      let nt = Scanf.scanf " %d" (fun x -> x) in
      let trans = Array.init nt (fun _ ->
        let s2 = Scanf.scanf " %d" (fun x -> x) in
        let n = Scanf.scanf " %f" (fun x -> x) in
        let d = Scanf.scanf " %f" (fun x -> x) in
        let r = Scanf.scanf " %f" (fun x -> x) in
        { s2; p = n /. d; r }
      ) in
      tr.(s).(a) <- trans
    done
  done;
  let gn = Scanf.scanf " %f" (fun x -> x) in
  let gd = Scanf.scanf " %f" (fun x -> x) in
  let an = Scanf.scanf " %f" (fun x -> x) in
  let ad = Scanf.scanf " %f" (fun x -> x) in
  let en = Scanf.scanf " %f" (fun x -> x) in
  let ed = Scanf.scanf " %f" (fun x -> x) in
  let g = gn /. gd in
  let al = an /. ad in
  let eps = en /. ed in
  let e_episodes = Scanf.scanf " %d" (fun x -> x) in
  let l_steps = Scanf.scanf " %d" (fun x -> x) in
  let seed = Scanf.scanf " %Ld" (fun x -> x) in

  let egreedy rng q s =
    let u = next_double rng in
    if u < eps then next_int rng a_count
    else begin
      let ba = ref 0 in
      let bv = ref q.(s).(0) in
      for a = 1 to a_count - 1 do
        if q.(s).(a) > !bv then begin
          bv := q.(s).(a);
          ba := a
        end
      done;
      !ba
    end
  in

  let step rng s a =
    let u = next_double rng in
    let c = ref 0.0 in
    let last = tr.(s).(a).(Array.length tr.(s).(a) - 1) in
    let ns = ref last.s2 in
    let rw = ref last.r in
    let found = ref false in
    let len = Array.length tr.(s).(a) in
    let i = ref 0 in
    while !i < len && not !found do
      let t = tr.(s).(a).(!i) in
      c := !c +. t.p;
      if u < !c then begin
        ns := t.s2;
        rw := t.r;
        found := true
      end;
      incr i
    done;
    (!ns, !rw)
  in

  (* Q-learning *)
  let rng = make_lcg seed in
  let q = Array.make_matrix s_count a_count 0.0 in
  for _ = 0 to e_episodes - 1 do
    let s = ref 0 in
    for _ = 0 to l_steps - 1 do
      let a = egreedy rng q !s in
      let ns, rw = step rng !s a in
      let mx = ref q.(ns).(0) in
      for b = 1 to a_count - 1 do
        if q.(ns).(b) > !mx then mx := q.(ns).(b)
      done;
      q.(!s).(a) <- q.(!s).(a) +. al *. (rw +. g *. !mx -. q.(!s).(a));
      s := ns
    done
  done;

  Printf.printf "QLEARN\n";
  for s = 0 to s_count - 1 do
    for a = 0 to a_count - 1 do
      if a > 0 then print_string " ";
      Printf.printf "%.6f" q.(s).(a)
    done;
    print_newline ()
  done;
  Printf.printf "QP";
  for s = 0 to s_count - 1 do
    let ba = ref 0 in
    for a = 1 to a_count - 1 do
      if q.(s).(a) > q.(s).(!ba) then ba := a
    done;
    Printf.printf " %d" !ba
  done;
  print_newline ();

  (* SARSA *)
  let rng2 = make_lcg seed in
  let q2 = Array.make_matrix s_count a_count 0.0 in
  for _ = 0 to e_episodes - 1 do
    let s = ref 0 in
    let a = ref (egreedy rng2 q2 0) in
    for _ = 0 to l_steps - 1 do
      let ns, rw = step rng2 !s !a in
      let na = egreedy rng2 q2 ns in
      q2.(!s).(!a) <- q2.(!s).(!a) +. al *. (rw +. g *. q2.(ns).(na) -. q2.(!s).(!a));
      s := ns;
      a := na
    done
  done;

  Printf.printf "SARSA\n";
  for s = 0 to s_count - 1 do
    for a = 0 to a_count - 1 do
      if a > 0 then print_string " ";
      Printf.printf "%.6f" q2.(s).(a)
    done;
    print_newline ()
  done;
  Printf.printf "SP";
  for s = 0 to s_count - 1 do
    let ba = ref 0 in
    for a = 1 to a_count - 1 do
      if q2.(s).(a) > q2.(s).(!ba) then ba := a
    done;
    Printf.printf " %d" !ba
  done;
  print_newline ()

let () =
  try
    let t = Scanf.scanf " %d" (fun x -> x) in
    for _ = 0 to t - 1 do
      solve_case ()
    done
  with End_of_file -> ()
