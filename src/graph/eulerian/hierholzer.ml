let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let m = Scanf.scanf " %d" (fun x -> x) in
    let adj = Array.make n [] in
    let in_deg = Array.make n 0 in
    let out_deg = Array.make n 0 in
    for _ = 1 to m do
      let u = Scanf.scanf " %d" (fun x -> x) in
      let v = Scanf.scanf " %d" (fun x -> x) in
      adj.(u) <- v :: adj.(u);
      out_deg.(u) <- out_deg.(u) + 1;
      in_deg.(v) <- in_deg.(v) + 1
    done;
    for i = 0 to n - 1 do
      adj.(i) <- List.sort compare adj.(i)
    done;
    let start_node = ref (-1) in
    let end_node = ref (-1) in
    let bad_nodes = ref 0 in
    for i = 0 to n - 1 do
      if out_deg.(i) - in_deg.(i) = 1 then begin
        if !start_node <> -1 then bad_nodes := 100;
        start_node := i
      end else if in_deg.(i) - out_deg.(i) = 1 then begin
        if !end_node <> -1 then bad_nodes := 100;
        end_node := i
      end else if in_deg.(i) <> out_deg.(i) then
        bad_nodes := 100
    done;
    let is_circuit = ref false in
    let valid = ref true in
    if !bad_nodes = 0 && !start_node = -1 && !end_node = -1 then begin
      is_circuit := true;
      let i = ref 0 in
      while !start_node = -1 && !i < n do
        if out_deg.(!i) > 0 then start_node := !i;
        incr i
      done;
      if !start_node = -1 then start_node := 0
    end else if !bad_nodes = 0 && !start_node <> -1 && !end_node <> -1 then begin
      is_circuit := false
    end else begin
      valid := false
    end;

    if not !valid then begin
      Printf.printf "NO EULERIAN PATH\n";
      exit 0
    end;

    let st = ref [!start_node] in
    let path = ref [] in
    while !st <> [] do
      let u = List.hd !st in
      match adj.(u) with
      | v :: rest ->
        adj.(u) <- rest;
        st := v :: !st
      | [] ->
        path := u :: !path;
        st := List.tl !st
    done;

    if List.length !path <> m + 1 then begin
      Printf.printf "NO EULERIAN PATH\n";
      exit 0
    end;

    if !is_circuit then
      Printf.printf "CIRCUIT\n"
    else
      Printf.printf "PATH\n";

    let first = ref true in
    List.iter (fun x ->
      if not !first then print_string " ";
      print_int x;
      first := false
    ) !path;
    print_newline ()
  with End_of_file -> ()
