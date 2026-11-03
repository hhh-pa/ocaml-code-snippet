let inf = 1_000_000_000_000_000_000L

let () =
  try
    let v_count = Scanf.scanf " %d" (fun x -> x) in
    let e_count = Scanf.scanf " %d" (fun x -> x) in
    let start_node = Scanf.scanf " %d" (fun x -> x) in
    let edges = Array.init e_count (fun _ ->
      let u = Scanf.scanf " %d" (fun x -> x) in
      let v = Scanf.scanf " %d" (fun x -> x) in
      let w = Scanf.scanf " %Ld" (fun x -> x) in
      (u, v, w)
    ) in
    let dist = Array.make v_count inf in
    let parent = Array.make v_count (-1) in
    dist.(start_node) <- 0L;

    let last_relaxed = ref (-1) in
    let rec relax iter =
      if iter < v_count then begin
        last_relaxed := -1;
        Array.iter (fun (u, v, w) ->
          if dist.(u) < inf then begin
            let cand = Int64.add dist.(u) w in
            if cand < dist.(v) then begin
              dist.(v) <- cand;
              parent.(v) <- u;
              last_relaxed := v
            end
          end
        ) edges;
        if !last_relaxed <> -1 then
          relax (iter + 1)
      end
    in
    relax 0;

    if !last_relaxed <> -1 then begin
      let cycle_node = ref !last_relaxed in
      for _ = 0 to v_count - 1 do
        cycle_node := parent.(!cycle_node)
      done;
      let start_c = !cycle_node in
      let cycle = ref [] in
      let cur = ref start_c in
      let rec collect () =
        cycle := !cur :: !cycle;
        if !cur = start_c && List.length !cycle > 1 then ()
        else begin
          cur := parent.(!cur);
          collect ()
        end
      in
      collect ();
      Printf.printf "NEGATIVE CYCLE\n";
      let first = ref true in
      List.iter (fun node ->
        if not !first then Printf.printf " ";
        Printf.printf "%d" node;
        first := false
      ) !cycle;
      Printf.printf "\n"
    end else begin
      let half_inf = Int64.div inf 2L in
      for i = 0 to v_count - 1 do
        if dist.(i) >= half_inf then
          Printf.printf "INF\n"
        else
          Printf.printf "%Ld\n" dist.(i)
      done
    end
  with End_of_file -> ()
