let inf = 4_000_000_000_000_000_000L

let rec gcd a b =
  if b = 0 then a else gcd b (a mod b)

let dinic n_nodes cap s t =
  let flow = ref 0L in
  let lvl = Array.make n_nodes (-1) in
  let it = Array.make n_nodes 0 in

  let rec dfs u f =
    if u = t then f
    else begin
      let res = ref 0L in
      while it.(u) < n_nodes && !res = 0L do
        let v = it.(u) in
        if lvl.(v) = lvl.(u) + 1 && cap.(u).(v) > 0L then begin
          let r = dfs v (min f cap.(u).(v)) in
          if r > 0L then begin
            cap.(u).(v) <- Int64.sub cap.(u).(v) r;
            cap.(v).(u) <- Int64.add cap.(v).(u) r;
            res := r
          end else
            it.(u) <- it.(u) + 1
        end else
          it.(u) <- it.(u) + 1
      done;
      !res
    end
  in

  let continue_bfs = ref true in
  while !continue_bfs do
    Array.fill lvl 0 n_nodes (-1);
    lvl.(s) <- 0;
    let q = Queue.create () in
    Queue.add s q;
    while not (Queue.is_empty q) do
      let u = Queue.take q in
      for v = 0 to n_nodes - 1 do
        if lvl.(v) = -1 && cap.(u).(v) > 0L then begin
          lvl.(v) <- lvl.(u) + 1;
          Queue.add v q
        end
      done
    done;
    if lvl.(t) = -1 then
      continue_bfs := false
    else begin
      Array.fill it 0 n_nodes 0;
      let rec push_loop () =
        let r = dfs s inf in
        if r > 0L then begin
          flow := Int64.add !flow r;
          push_loop ()
        end
      in
      push_loop ()
    end
  done;
  !flow

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let m = Scanf.scanf " %d" (fun x -> x) in
    let deg = Array.make n 0 in
    let es = Array.init m (fun _ ->
      let u = Scanf.scanf " %d" (fun x -> x) in
      let v = Scanf.scanf " %d" (fun x -> x) in
      deg.(u) <- deg.(u) + 1;
      deg.(v) <- deg.(v) + 1;
      (u, v)
    ) in

    let big_n = n + 2 in
    let s = n in
    let t = n + 1 in

    let cands = ref [] in
    for p = 0 to m do
      for q = 1 to n do
        cands := (p, q) :: !cands
      done
    done;

    let cmp_cand (p1, q1) (p2, q2) =
      let l = p1 * q2 in
      let r = p2 * q1 in
      if l <> r then compare l r
      else
        let c = compare p1 p2 in
        if c <> 0 then c
        else compare q1 q2
    in
    let sorted_cands = List.sort cmp_cand !cands in

    let rec unique = function
      | [] -> []
      | [x] -> [x]
      | x :: (y :: _ as tl) ->
          if cmp_cand x y = 0 then unique tl
          else x :: unique tl
    in
    let unique_cands = unique sorted_cands in

    let solved = ref false in
    List.iter (fun (p, q) ->
      if not !solved then begin
        let cap = Array.make_matrix big_n big_n 0L in
        let mq = Int64.of_int (m * q) in
        for v = 0 to n - 1 do
          cap.(s).(v) <- Int64.add cap.(s).(v) mq;
          let cap_vt = Int64.of_int (m * q + 2 * p - q * deg.(v)) in
          cap.(v).(t) <- Int64.add cap.(v).(t) cap_vt
        done;
        let q_64 = Int64.of_int q in
        Array.iter (fun (u, v) ->
          cap.(u).(v) <- Int64.add cap.(u).(v) q_64;
          cap.(v).(u) <- Int64.add cap.(v).(u) q_64
        ) es;
        let target = Int64.mul (Int64.of_int q) (Int64.of_int (n * m)) in
        if dinic big_n cap s t = target then begin
          let g = gcd p q in
          Printf.printf "%d/%d\n" (p / g) (q / g);
          solved := true
        end
      end
    ) unique_cands;

    if not !solved then
      Printf.printf "0/1\n"
  with End_of_file -> ()
