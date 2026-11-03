let () =
  try
    let mode = Scanf.scanf " %s" (fun s -> s) in
    if mode = "VC" then begin
      let n = Scanf.scanf " %d" (fun x -> x) in
      let m = Scanf.scanf " %d" (fun x -> x) in
      let u_arr = Array.make m 0 in
      let v_arr = Array.make m 0 in
      for i = 0 to m - 1 do
        u_arr.(i) <- Scanf.scanf " %d" (fun x -> x);
        v_arr.(i) <- Scanf.scanf " %d" (fun x -> x)
      done;
      let cov = Array.make n false in
      for i = 0 to m - 1 do
        let u = u_arr.(i) in
        let v = v_arr.(i) in
        if not cov.(u) && not cov.(v) then begin
          cov.(u) <- true;
          cov.(v) <- true
        end
      done;
      let out = ref [] in
      for i = 0 to n - 1 do
        if cov.(i) then out := i :: !out
      done;
      let out_list = List.rev !out in
      Printf.printf "%d\n" (List.length out_list);
      let first = ref true in
      List.iter (fun x ->
        if not !first then print_string " ";
        first := false;
        print_int x
      ) out_list;
      print_newline ()
    end else if mode = "SC" then begin
      let n = Scanf.scanf " %d" (fun x -> x) in
      let m = Scanf.scanf " %d" (fun x -> x) in
      let sets = Array.init m (fun _ ->
        let k = Scanf.scanf " %d" (fun x -> x) in
        Array.init k (fun _ -> Scanf.scanf " %d" (fun x -> x))
      ) in
      let unc = Array.make n true in
      let left = ref n in
      let chosen = ref [] in
      while !left > 0 do
        let best = ref (-1) in
        let bi = ref (-1) in
        for i = 0 to m - 1 do
          let cnt = ref 0 in
          Array.iter (fun e -> if unc.(e) then incr cnt) sets.(i);
          if !cnt > !best then begin
            best := !cnt;
            bi := i
          end
        done;
        if !best <= 0 then
          left := 0
        else begin
          chosen := !bi :: !chosen;
          Array.iter (fun e ->
            if unc.(e) then begin
              unc.(e) <- false;
              decr left
            end
          ) sets.(!bi)
        end
      done;
      let ch_list = List.rev !chosen in
      Printf.printf "%d\n" (List.length ch_list);
      let first = ref true in
      List.iter (fun x ->
        if not !first then print_string " ";
        first := false;
        print_int x
      ) ch_list;
      print_newline ()
    end else if mode = "TSP" then begin
      let n = Scanf.scanf " %d" (fun x -> x) in
      let d = Array.make_matrix n n 0 in
      for i = 0 to n - 1 do
        for j = 0 to n - 1 do
          d.(i).(j) <- Scanf.scanf " %d" (fun x -> x)
        done
      done;
      let vis = Array.make n false in
      let tour = ref [0] in
      vis.(0) <- true;
      let cur = ref 0 in
      let total = ref 0 in
      for _ = 0 to n - 2 do
        let best = ref (-1) in
        let bi = ref (-1) in
        for j = 0 to n - 1 do
          if not vis.(j) && (!best < 0 || d.(!cur).(j) < !best) then begin
            best := d.(!cur).(j);
            bi := j
          end
        done;
        tour := !bi :: !tour;
        vis.(!bi) <- true;
        total := !total + !best;
        cur := !bi
      done;
      total := !total + d.(!cur).(0);
      tour := 0 :: !tour;
      let tour_list = List.rev !tour in
      let first = ref true in
      List.iter (fun x ->
        if not !first then print_string " ";
        first := false;
        print_int x
      ) tour_list;
      print_newline ();
      Printf.printf "%d\n" !total
    end else if mode = "SS" then begin
      let n = Scanf.scanf " %d" (fun x -> x) in
      let target = Scanf.scanf " %d" (fun x -> x) in
      let eps = Scanf.scanf " %f" (fun x -> x) in
      let vals = Array.init n (fun _ -> Scanf.scanf " %d" (fun x -> x)) in
      let delta = eps /. (2.0 *. float_of_int n) in
      module IntSet = Set.Make(Int) in
      let l = ref [0] in
      Array.iter (fun x ->
        let m_set = ref (List.fold_left (fun acc v -> IntSet.add v acc) IntSet.empty !l) in
        List.iter (fun v ->
          if v + x <= target then m_set := IntSet.add (v + x) !m_set
        ) !l;
        let mv = IntSet.elements !m_set in
        let nl = ref [List.hd mv] in
        let last = ref (float_of_int (List.hd mv)) in
        List.iter (fun elem ->
          let elem_f = float_of_int elem in
          if elem_f > !last *. (1.0 +. delta) then begin
            nl := elem :: !nl;
            last := elem_f
          end
        ) (List.tl mv);
        l := List.rev !nl
      ) vals;
      let ans = List.hd (List.rev !l) in
      Printf.printf "%d\n" ans
    end
  with End_of_file -> ()
