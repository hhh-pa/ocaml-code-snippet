let bfs_depth adj s =
  let n = Array.length adj in
  if n = 0 then 0
  else begin
    let d = Array.make n (-1) in
    let q = Queue.create () in
    d.(s) <- 0;
    Queue.push s q;
    let mx = ref 0 in
    while not (Queue.is_empty q) do
      let u = Queue.pop q in
      List.iter (fun v ->
        if d.(v) < 0 then begin
          d.(v) <- d.(u) + 1;
          if d.(v) > !mx then mx := d.(v);
          Queue.push v q
        end
      ) adj.(u)
    done;
    !mx
  end

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let m = Scanf.scanf " %d" (fun x -> x) in
    let adj = Array.make n [] in
    let edges = Array.init m (fun _ ->
      let u = Scanf.scanf " %d" (fun x -> x) in
      let v = Scanf.scanf " %d" (fun x -> x) in
      adj.(u) <- v :: adj.(u);
      adj.(v) <- u :: adj.(v);
      (u, v)
    ) in
    let p = Scanf.scanf " %Ld" (fun x -> x) in
    let nc = Scanf.scanf " %d" (fun x -> x) in
    let cl = Array.init n (fun _ -> Scanf.scanf " %d" (fun x -> x)) in
    let cadj = Array.make nc [] in
    for _ = 0 to nc - 2 do
      let a = Scanf.scanf " %d" (fun x -> x) in
      let b = Scanf.scanf " %d" (fun x -> x) in
      cadj.(a) <- b :: cadj.(a);
      cadj.(b) <- a :: cadj.(b)
    done;
    let depth = bfs_depth adj 0 in
    let cdepth = if nc > 0 then bfs_depth cadj 0 else 0 in
    let minintra = ref 0L in
    for i = 0 to m - 1 do
      let (u, v) = edges.(i) in
      if cl.(u) = cl.(v) then
        minintra := Int64.add !minintra 1L
    done;
    let alpha_msgs = Int64.mul (Int64.mul 2L (Int64.of_int m)) p in
    let beta_msgs = Int64.mul (Int64.mul 2L (Int64.of_int (n - 1))) p in
    let beta_time = Int64.mul (Int64.mul 2L (Int64.of_int depth)) p in
    let gamma_msgs =
      let inner = Int64.add (Int64.mul 2L !minintra) (Int64.mul 2L (Int64.of_int (nc - 1))) in
      Int64.mul inner p
    in
    let gamma_time =
      let inner = Int64.add 2L (Int64.mul 2L (Int64.of_int cdepth)) in
      Int64.mul inner p
    in
    Printf.printf "ALPHA MESSAGES: %Ld TIME: %Ld\n" alpha_msgs p;
    Printf.printf "BETA MESSAGES: %Ld TIME: %Ld\n" beta_msgs beta_time;
    Printf.printf "GAMMA MESSAGES: %Ld TIME: %Ld\n" gamma_msgs gamma_time
  with End_of_file -> ()
