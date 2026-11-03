let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let s = Scanf.scanf " %d" (fun x -> x) in
    if n > 0 && s >= 0 && s < n then begin
      let nxt = Array.init n (fun _ -> Scanf.scanf " %d" (fun x -> x)) in
      let tortoise = ref s in
      let hare = ref s in
      let has_cycle = ref false in
      let finished = ref false in
      while not !finished do
        if !hare = -1 || nxt.(!hare) = -1 then begin
          has_cycle := false;
          finished := true
        end else begin
          let next_hare = nxt.(!hare) in
          if nxt.(next_hare) = -1 then begin
            has_cycle := false;
            finished := true
          end else begin
            tortoise := nxt.(!tortoise);
            hare := nxt.(next_hare);
            if !tortoise = !hare then begin
              has_cycle := true;
              finished := true
            end
          end
        end
      done;
      if not !has_cycle then begin
        let steps = ref 0 in
        let cur = ref s in
        while !cur <> -1 do
          cur := nxt.(!cur);
          incr steps
        done;
        Printf.printf "NO CYCLE %d\n" !steps
      end else begin
        let mu = ref 0 in
        tortoise := s;
        while !tortoise <> !hare do
          tortoise := nxt.(!tortoise);
          hare := nxt.(!hare);
          incr mu
        done;
        let entry_node = !tortoise in
        let lam = ref 1 in
        hare := nxt.(!tortoise);
        while !tortoise <> !hare do
          hare := nxt.(!hare);
          incr lam
        done;
        Printf.printf "CYCLE %d %d %d\n" !mu !lam entry_node
      end
    end
  with End_of_file -> ()
