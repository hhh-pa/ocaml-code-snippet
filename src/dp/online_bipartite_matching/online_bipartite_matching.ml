let () =
  try
    let nU = Scanf.scanf " %d" (fun x -> x) in
    let nV = Scanf.scanf " %d" (fun x -> x) in
    let m = Scanf.scanf " %d" (fun x -> x) in
    let adj = Array.make nU [] in
    for _ = 1 to m do
      let u = Scanf.scanf " %d" (fun x -> x) in
      let v = Scanf.scanf " %d" (fun x -> x) in
      adj.(u) <- v :: adj.(u)
    done;
    for u = 0 to nU - 1 do
      adj.(u) <- List.rev adj.(u)
    done;
    let x = ref 42L in
    let mod31 = Int64.shift_left 1L 31 in
    let rnd n =
      x := Int64.rem (Int64.add (Int64.mul 1103515245L !x) 12345L) mod31;
      Int64.to_int (Int64.rem !x (Int64.of_int n))
    in
    let rk = Array.init nV (fun i -> i) in
    for i = nV - 1 downto 1 do
      let j = rnd (i + 1) in
      let tmp = rk.(i) in
      rk.(i) <- rk.(j);
      rk.(j) <- tmp
    done;
    let pos = Array.make nV 0 in
    for r = 0 to nV - 1 do
      pos.(rk.(r)) <- r
    done;
    let mate = Array.make nU (-1) in
    let used = Array.make nV false in
    for u = 0 to nU - 1 do
      let best = ref (-1) in
      let bp = ref (nV + 1) in
      List.iter (fun v ->
        if not used.(v) && pos.(v) < !bp then begin
          best := v;
          bp := pos.(v)
        end
      ) adj.(u);
      if !best <> -1 then begin
        mate.(u) <- !best;
        used.(!best) <- true
      end
    done;
    let pr = ref [] in
    for u = 0 to nU - 1 do
      if mate.(u) <> -1 then pr := (u, mate.(u)) :: !pr
    done;
    let pr_list = List.rev !pr in
    Printf.printf "%d\n" (List.length pr_list);
    List.iter (fun (u, v) ->
      Printf.printf "%d %d\n" u v
    ) pr_list
  with End_of_file -> ()
