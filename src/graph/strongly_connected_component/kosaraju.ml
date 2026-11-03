let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let _m = Scanf.scanf " %d" (fun x -> x) in
    let graph = Array.make n [] in
    let rev_graph = Array.make n [] in
    let rec read_edges () =
      try
        let a = Scanf.scanf " %d" (fun x -> x) - 1 in
        let b = Scanf.scanf " %d" (fun x -> x) - 1 in
        graph.(a) <- b :: graph.(a);
        rev_graph.(b) <- a :: rev_graph.(b);
        read_edges ()
      with End_of_file -> ()
    in
    read_edges ();

    let marked = Array.make n false in
    let post_order = ref [] in
    let rec dfs1 v =
      marked.(v) <- true;
      List.iter (fun w ->
        if not marked.(w) then dfs1 w
      ) rev_graph.(v);
      post_order := v :: !post_order
    in
    for v = 0 to n - 1 do
      if not marked.(v) then dfs1 v
    done;

    let marked2 = Array.make n false in
    let components = Array.make n (-1) in
    let count = ref 0 in
    let rec dfs2 v c =
      marked2.(v) <- true;
      components.(v) <- c;
      List.iter (fun w ->
        if not marked2.(w) then dfs2 w c
      ) graph.(v)
    in
    List.iter (fun v ->
      if not marked2.(v) then begin
        dfs2 v !count;
        incr count
      end
    ) !post_order;

    let counts = Array.make !count 0 in
    for v = 0 to n - 1 do
      let c = components.(v) in
      if c >= 0 then
        counts.(c) <- counts.(c) + 1
    done;

    let odd = ref 0 in
    let even = ref 0 in
    for c = 0 to !count - 1 do
      let sz = counts.(c) in
      if sz mod 2 <> 0 then odd := !odd + sz
      else even := !even + sz
    done;

    Printf.printf "%d\n" (!odd - !even)
  with End_of_file -> ()
