let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let m = Scanf.scanf " %d" (fun x -> x) in
    let adj = Array.make n [] in
    for _ = 0 to m - 1 do
      let u = Scanf.scanf " %d" (fun x -> x) in
      let v = Scanf.scanf " %d" (fun x -> x) in
      adj.(u) <- v :: adj.(u);
      adj.(v) <- u :: adj.(v)
    done;
    let id_arr = Array.init n (fun _ -> Scanf.scanf " %Ld" (fun x -> x)) in
    let mx = ref id_arr.(0) in
    for i = 1 to n - 1 do
      if id_arr.(i) > !mx then mx := id_arr.(i)
    done;
    let r_diam = ref 0 in
    for s = 0 to n - 1 do
      let d = Array.make n (-1) in
      let q = Queue.create () in
      d.(s) <- 0;
      Queue.push s q;
      while not (Queue.is_empty q) do
        let u = Queue.pop q in
        List.iter (fun v ->
          if d.(v) < 0 then begin
            d.(v) <- d.(u) + 1;
            Queue.push v q
          end
        ) adj.(u)
      done;
      for i = 0 to n - 1 do
        if d.(i) > !r_diam then r_diam := d.(i)
      done
    done;
    let seen = Array.copy id_arr in
    let opt = ref 0L in
    let send = Array.make n 1 in
    for _ = 1 to !r_diam do
      let ns = Array.copy seen in
      for i = 0 to n - 1 do
        if send.(i) = 1 then
          opt := Int64.add !opt (Int64.of_int (List.length adj.(i)));
        List.iter (fun nb ->
          if send.(nb) = 1 && seen.(nb) > ns.(i) then
            ns.(i) <- seen.(nb)
        ) adj.(i)
      done;
      for i = 0 to n - 1 do
        send.(i) <- if ns.(i) <> seen.(i) then 1 else 0
      done;
      Array.blit ns 0 seen 0 n
    done;
    let floodmax_msgs = Int64.mul (Int64.mul 2L (Int64.of_int m)) (Int64.of_int !r_diam) in
    Printf.printf "LEADER: %Ld\n" !mx;
    Printf.printf "ROUNDS: %d\n" !r_diam;
    Printf.printf "FLOODMAX MESSAGES: %Ld\n" floodmax_msgs;
    Printf.printf "OPTFLOODMAX MESSAGES: %Ld\n" !opt
  with End_of_file -> ()
