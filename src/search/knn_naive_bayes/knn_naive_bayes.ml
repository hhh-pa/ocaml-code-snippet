let () =
  try
    let t_cases = Scanf.scanf " %d" (fun x -> x) in
    for _ = 1 to t_cases do
      let n = Scanf.scanf " %d" (fun x -> x) in
      let f = Scanf.scanf " %d" (fun x -> x) in
      let k = Scanf.scanf " %d" (fun x -> x) in

      let x_mat = Array.make_matrix n f 0 in
      let y_vec = Array.make n 0 in
      for i = 0 to n - 1 do
        for j = 0 to f - 1 do
          x_mat.(i).(j) <- Scanf.scanf " %d" (fun x -> x)
        done;
        y_vec.(i) <- Scanf.scanf " %d" (fun x -> x)
      done;

      let m = Scanf.scanf " %d" (fun x -> x) in
      let z_mat = Array.make_matrix m f 0 in
      for i = 0 to m - 1 do
        for j = 0 to f - 1 do
          z_mat.(i).(j) <- Scanf.scanf " %d" (fun x -> x)
        done
      done;

      (* KNN *)
      print_string "KNN";
      for t = 0 to m - 1 do
        let d = Array.init n (fun i ->
          let s = ref 0L in
          for j = 0 to f - 1 do
            let diff = Int64.of_int (z_mat.(t).(j) - x_mat.(i).(j)) in
            s := Int64.add !s (Int64.mul diff diff)
          done;
          (!s, i)
        ) in
        Array.sort (fun (s1, i1) (s2, i2) ->
          let c = compare s1 s2 in
          if c <> 0 then c else compare i1 i2
        ) d;

        let votes = Hashtbl.create k in
        let limit = min k n in
        for j = 0 to limit - 1 do
          let lab = y_vec.(snd d.(j)) in
          let cur = match Hashtbl.find_opt votes lab with Some v -> v | None -> 0 in
          Hashtbl.replace votes lab (cur + 1)
        done;

        let unique_labels = ref [] in
        Hashtbl.iter (fun lab _ -> unique_labels := lab :: !unique_labels) votes;
        let sorted_labels = List.sort compare !unique_labels in
        let bl = ref (-1) in
        let bc = ref (-1) in
        List.iter (fun lab ->
          let cnt = Hashtbl.find votes lab in
          if cnt > !bc then begin
            bc := cnt;
            bl := lab
          end
        ) sorted_labels;
        Printf.printf " %d" !bl
      done;
      print_newline ();

      (* Naive Bayes *)
      print_string "NB";
      let classes_list = ref [] in
      for i = 0 to n - 1 do
        classes_list := y_vec.(i) :: !classes_list
      done;
      let sorted_unique_classes =
        List.sort_uniq compare !classes_list
      in
      let vmax = Array.make f 0 in
      for j = 0 to f - 1 do
        for i = 0 to n - 1 do
          vmax.(j) <- max vmax.(j) x_mat.(i).(j)
        done
      done;

      for t = 0 to m - 1 do
        let bl = ref (List.hd sorted_unique_classes) in
        let bs = ref (-1e300) in
        List.iter (fun c ->
          let cc = ref 0 in
          for i = 0 to n - 1 do
            if y_vec.(i) = c then incr cc
          done;
          let s = ref (log (float_of_int !cc /. float_of_int n)) in
          for j = 0 to f - 1 do
            let cnt = ref 0 in
            for i = 0 to n - 1 do
              if y_vec.(i) = c && x_mat.(i).(j) = z_mat.(t).(j) then incr cnt
            done;
            let num = float_of_int (!cnt + 1) in
            let den = float_of_int (!cc + vmax.(j) + 1) in
            s := !s +. log (num /. den)
          done;
          if !s > !bs +. 1e-12 then begin
            bs := !s;
            bl := c
          end
        ) sorted_unique_classes;
        Printf.printf " %d" !bl
      done;
      print_newline ()
    done
  with End_of_file -> ()
