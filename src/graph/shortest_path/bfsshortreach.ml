let () =
  try
    let t = Scanf.scanf " %d" (fun x -> x) in
    for _ = 0 to t - 1 do
      let n = Scanf.scanf " %d" (fun x -> x) in
      let m = Scanf.scanf " %d" (fun x -> x) in
      let adj = Array.make (n + 1) [] in
      for _ = 0 to m - 1 do
        let u = Scanf.scanf " %d" (fun x -> x) in
        let v = Scanf.scanf " %d" (fun x -> x) in
        adj.(u) <- v :: adj.(u);
        adj.(v) <- u :: adj.(v)
      done;
      let start = Scanf.scanf " %d" (fun x -> x) in
      let dist = Array.make (n + 1) (-1) in
      dist.(start) <- 0;
      let q = Queue.create () in
      Queue.add start q;
      while not (Queue.is_empty q) do
        let u = Queue.take q in
        let d_next = dist.(u) + 6 in
        List.iter (fun v ->
          if dist.(v) = -1 then begin
            dist.(v) <- d_next;
            Queue.add v q
          end
        ) adj.(u)
      done;
      let first = ref true in
      for i = 1 to n do
        if i <> start then begin
          if not !first then Printf.printf " ";
          Printf.printf "%d" dist.(i);
          first := false
        end
      done;
      Printf.printf "\n"
    done
  with End_of_file -> ()
