let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let m = Scanf.scanf " %d" (fun x -> x) in
    let adj = Array.make (n + 1) [] in
    for _ = 1 to m do
      let u = Scanf.scanf " %d" (fun x -> x) in
      let v = Scanf.scanf " %d" (fun x -> x) in
      adj.(u) <- v :: adj.(u);
      adj.(v) <- u :: adj.(v)
    done;
    let cuts = ref 0 in
    let rec dfs u parent =
      let sz = ref 1 in
      List.iter (fun v ->
        if v <> parent then
          sz := !sz + dfs v u
      ) adj.(u);
      if !sz mod 2 = 0 && parent <> 0 then
        incr cuts;
      !sz
    in
    ignore (dfs 1 0);
    Printf.printf "%d\n" !cuts
  with End_of_file -> ()
