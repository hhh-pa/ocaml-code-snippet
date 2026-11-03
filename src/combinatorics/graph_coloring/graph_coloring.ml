module IntSet = Set.Make(Int)

let () =
  try
    let mode = Scanf.scanf " %s" (fun s -> s) in
    let n = Scanf.scanf " %d" (fun x -> x) in
    let m = Scanf.scanf " %d" (fun x -> x) in
    let adj = Array.make n [] in
    let deg = Array.make n 0 in
    for _ = 1 to m do
      let u = Scanf.scanf " %d" (fun x -> x) in
      let v = Scanf.scanf " %d" (fun x -> x) in
      adj.(u) <- v :: adj.(u);
      adj.(v) <- u :: adj.(v);
      deg.(u) <- deg.(u) + 1;
      deg.(v) <- deg.(v) + 1
    done;
    let order = Array.init n (fun i -> i) in
    if mode = "W" then
      Array.sort (fun a b ->
        if deg.(a) <> deg.(b) then compare deg.(b) deg.(a)
        else compare a b
      ) order;
    let color = Array.make n (-1) in
    for idx = 0 to n - 1 do
      let u = order.(idx) in
      let used = ref IntSet.empty in
      List.iter (fun w ->
        if color.(w) >= 0 then used := IntSet.add color.(w) !used
      ) adj.(u);
      let c = ref 0 in
      while IntSet.mem !c !used do
        incr c
      done;
      color.(u) <- !c
    done;
    let max_col = ref (-1) in
    for i = 0 to n - 1 do
      if color.(i) > !max_col then max_col := color.(i)
    done;
    let nc = !max_col + 1 in
    Printf.printf "%d\n" nc;
    for i = 0 to n - 1 do
      if i > 0 then print_char ' ';
      print_int color.(i)
    done;
    print_newline ()
  with End_of_file -> ()
