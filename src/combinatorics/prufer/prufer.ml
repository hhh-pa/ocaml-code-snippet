module IntSet = Set.Make(Int)

let () =
  try
    let mode = Scanf.scanf " %s" (fun s -> s) in
    if mode = "E" then begin
      let n = Scanf.scanf " %d" (fun x -> x) in
      let adj = Array.make n IntSet.empty in
      let deg = Array.make n 0 in
      for _ = 1 to n - 1 do
        let u = Scanf.scanf " %d" (fun x -> x) in
        let v = Scanf.scanf " %d" (fun x -> x) in
        adj.(u) <- IntSet.add v adj.(u);
        adj.(v) <- IntSet.add u adj.(v);
        deg.(u) <- deg.(u) + 1;
        deg.(v) <- deg.(v) + 1
      done;
      let leaves = ref IntSet.empty in
      for i = 0 to n - 1 do
        if deg.(i) = 1 then leaves := IntSet.add i !leaves
      done;
      let seq = ref [] in
      for _ = 1 to n - 2 do
        let leaf = IntSet.min_elt !leaves in
        leaves := IntSet.remove leaf !leaves;
        let nb = IntSet.min_elt adj.(leaf) in
        seq := nb :: !seq;
        adj.(leaf) <- IntSet.remove nb adj.(leaf);
        adj.(nb) <- IntSet.remove leaf adj.(nb);
        deg.(nb) <- deg.(nb) - 1;
        if deg.(nb) = 1 then leaves := IntSet.add nb !leaves
      done;
      let seq = List.rev !seq in
      List.iteri (fun i v ->
        if i > 0 then print_char ' ';
        print_int v
      ) seq;
      print_newline ()
    end else begin
      let n = Scanf.scanf " %d" (fun x -> x) in
      let seq_len = max 0 (n - 2) in
      let seq = Array.init seq_len (fun _ -> Scanf.scanf " %d" (fun x -> x)) in
      let deg = Array.make n 1 in
      for i = 0 to seq_len - 1 do
        let v = seq.(i) in
        deg.(v) <- deg.(v) + 1
      done;
      let leaves = ref IntSet.empty in
      for i = 0 to n - 1 do
        if deg.(i) = 1 then leaves := IntSet.add i !leaves
      done;
      let edges = ref [] in
      for i = 0 to seq_len - 1 do
        let v = seq.(i) in
        let leaf = IntSet.min_elt !leaves in
        leaves := IntSet.remove leaf !leaves;
        edges := (min leaf v, max leaf v) :: !edges;
        deg.(v) <- deg.(v) - 1;
        if deg.(v) = 1 then leaves := IntSet.add v !leaves
      done;
      let a = IntSet.min_elt !leaves in
      leaves := IntSet.remove a !leaves;
      let b = IntSet.min_elt !leaves in
      edges := (min a b, max a b) :: !edges;
      let sorted_edges = List.sort compare !edges in
      List.iter (fun (u, v) ->
        Printf.printf "%d %d\n" u v
      ) sorted_edges
    end
  with End_of_file -> ()
