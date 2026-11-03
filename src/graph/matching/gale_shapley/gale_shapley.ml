let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let mp = Array.make_matrix n n 0 in
    let wp = Array.make_matrix n n 0 in
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        mp.(i).(j) <- Scanf.scanf " %d" (fun x -> x)
      done
    done;
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        wp.(i).(j) <- Scanf.scanf " %d" (fun x -> x)
      done
    done;
    let rank = Array.make_matrix n n 0 in
    for w = 0 to n - 1 do
      for r = 0 to n - 1 do
        rank.(w).(wp.(w).(r)) <- r
      done
    done;
    let wife = Array.make n (-1) in
    let husb = Array.make n (-1) in
    let nxt = Array.make n 0 in
    let q = Queue.create () in
    for i = 0 to n - 1 do
      Queue.push i q
    done;
    while not (Queue.is_empty q) do
      let m = Queue.pop q in
      let w = mp.(m).(nxt.(m)) in
      nxt.(m) <- nxt.(m) + 1;
      let curr_h = husb.(w) in
      if curr_h = -1 then begin
        wife.(m) <- w;
        husb.(w) <- m
      end else if rank.(w).(m) < rank.(w).(curr_h) then begin
        wife.(curr_h) <- -1;
        Queue.push curr_h q;
        wife.(m) <- w;
        husb.(w) <- m
      end else begin
        Queue.push m q
      end
    done;
    for m = 0 to n - 1 do
      Printf.printf "%d %d\n" m wife.(m)
    done
  with End_of_file -> ()
