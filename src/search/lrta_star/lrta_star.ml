let () =
  try
    let t_cases = Scanf.scanf " %d" (fun x -> x) in
    for _ = 1 to t_cases do
      let r_cnt = Scanf.scanf " %d" (fun x -> x) in
      let c_cnt = Scanf.scanf " %d" (fun x -> x) in
      let grid = Array.make_matrix r_cnt c_cnt 0 in
      for i = 0 to r_cnt - 1 do
        for j = 0 to c_cnt - 1 do
          grid.(i).(j) <- Scanf.scanf " %d" (fun x -> x)
        done
      done;
      let sr = Scanf.scanf " %d" (fun x -> x) in
      let sc = Scanf.scanf " %d" (fun x -> x) in
      let gr = Scanf.scanf " %d" (fun x -> x) in
      let gc = Scanf.scanf " %d" (fun x -> x) in

      let h_man r c = abs (r - gr) + abs (c - gc) in
      let h_table = Hashtbl.create 1024 in
      let get_h r c =
        match Hashtbl.find_opt h_table (r, c) with
        | Some v -> v
        | None -> h_man r c
      in

      let dr = [| -1; 1; 0; 0 |] in
      let dc = [| 0; 0; -1; 1 |] in

      let cr = ref sr in
      let cc = ref sc in
      let steps = ref 0 in
      let path = ref [(!cr, !cc)] in
      let max_steps = 10000 in

      let stopped = ref false in
      while not (!cr = gr && !cc = gc) && !steps < max_steps && not !stopped do
        let nbrs = ref [] in
        for d = 0 to 3 do
          let nr = !cr + dr.(d) in
          let nc = !cc + dc.(d) in
          if nr >= 0 && nr < r_cnt && nc >= 0 && nc < c_cnt && grid.(nr).(nc) = 0 then
            nbrs := (nr, nc) :: !nbrs
        done;
        let nbrs_list = List.rev !nbrs in
        if nbrs_list = [] then
          stopped := true
        else begin
          let min_h = ref 1_000_000_000 in
          List.iter (fun (nr, nc) ->
            let v = 1 + get_h nr nc in
            if v < !min_h then min_h := v
          ) nbrs_list;
          Hashtbl.replace h_table (!cr, !cc) !min_h;

          let best = ref (List.hd nbrs_list) in
          let best_h = ref (1 + get_h (fst !best) (snd !best)) in
          List.iter (fun (nr, nc) ->
            let hv = 1 + get_h nr nc in
            if hv < !best_h || (hv = !best_h && (nr < fst !best || (nr = fst !best && nc < snd !best))) then begin
              best_h := hv;
              best := (nr, nc)
            end
          ) (List.tl nbrs_list);

          cr := fst !best;
          cc := snd !best;
          path := (!cr, !cc) :: !path;
          incr steps
        end
      done;

      Printf.printf "%d\n" !steps;
      let rev_path = List.rev !path in
      let first = ref true in
      List.iter (fun (r, c) ->
        if not !first then print_string " ";
        Printf.printf "%d %d" r c;
        first := false
      ) rev_path;
      print_newline ()
    done
  with End_of_file -> ()
