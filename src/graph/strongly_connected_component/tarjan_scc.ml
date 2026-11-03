let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let m = Scanf.scanf " %d" (fun x -> x) in
    let adj = Array.make n [] in
    for _ = 1 to m do
      let u = Scanf.scanf " %d" (fun x -> x) in
      let v = Scanf.scanf " %d" (fun x -> x) in
      adj.(u) <- v :: adj.(u)
    done;

    let dfn = Array.make n 0 in
    let low = Array.make n 0 in
    let on_stack = Array.make n false in
    let st = ref [] in
    let timer = ref 0 in
    let sccs = ref [] in

    let rec dfs u =
      incr timer;
      dfn.(u) <- !timer;
      low.(u) <- !timer;
      st := u :: !st;
      on_stack.(u) <- true;
      List.iter (fun v ->
        if dfn.(v) = 0 then begin
          dfs v;
          if low.(v) < low.(u) then low.(u) <- low.(v)
        end else if on_stack.(v) then begin
          if dfn.(v) < low.(u) then low.(u) <- dfn.(v)
        end
      ) adj.(u);

      if low.(u) = dfn.(u) then begin
        let scc = ref [] in
        let rec pop_scc () =
          match !st with
          | [] -> ()
          | node :: rest ->
            st := rest;
            on_stack.(node) <- false;
            scc := node :: !scc;
            if node <> u then pop_scc ()
        in
        pop_scc ();
        let sorted_scc = List.sort compare !scc in
        sccs := sorted_scc :: !sccs
      end
    in

    for i = 0 to n - 1 do
      if dfn.(i) = 0 then dfs i
    done;

    let sorted_sccs = List.sort (fun s1 s2 ->
      match s1, s2 with
      | h1 :: _, h2 :: _ -> compare h1 h2
      | [], _ -> -1
      | _, [] -> 1
    ) !sccs in

    Printf.printf "%d\n" (List.length sorted_sccs);
    List.iter (fun scc ->
      let first = ref true in
      List.iter (fun node ->
        if not !first then print_string " ";
        print_int node;
        first := false
      ) scc;
      print_newline ()
    ) sorted_sccs
  with End_of_file -> ()
