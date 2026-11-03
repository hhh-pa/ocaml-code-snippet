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
    for u = 0 to n - 1 do
      adj.(u) <- List.rev adj.(u)
    done;

    let pre = Array.make n 0 in
    let comp = Array.make n (-1) in
    let s_stack = ref [] in
    let p_stack = ref [] in
    let c = ref 0 in

    let rec dfs v =
      incr c;
      pre.(v) <- !c;
      s_stack := v :: !s_stack;
      p_stack := v :: !p_stack;
      List.iter (fun w ->
        if pre.(w) = 0 then
          dfs w
        else if comp.(w) = -1 then begin
          while !p_stack <> [] && pre.(List.hd !p_stack) > pre.(w) do
            p_stack := List.tl !p_stack
          done
        end
      ) adj.(v);
      if !p_stack <> [] && List.hd !p_stack = v then begin
        p_stack := List.tl !p_stack;
        let rec pop_s () =
          match !s_stack with
          | [] -> ()
          | x :: rest ->
            s_stack := rest;
            comp.(x) <- v;
            if x <> v then pop_s ()
        in
        pop_s ()
      end
    in

    for v = 0 to n - 1 do
      if pre.(v) = 0 then dfs v
    done;

    let min_elem = Hashtbl.create n in
    let comp_set = Hashtbl.create n in
    for v = 0 to n - 1 do
      let cv = comp.(v) in
      Hashtbl.replace comp_set cv true;
      match Hashtbl.find_opt min_elem cv with
      | None -> Hashtbl.add min_elem cv v
      | Some old_min -> if v < old_min then Hashtbl.replace min_elem cv v
    done;

    Printf.printf "%d\n" (Hashtbl.length comp_set);
    for v = 0 to n - 1 do
      if v > 0 then print_string " ";
      print_int (Hashtbl.find min_elem comp.(v))
    done;
    print_newline ()
  with End_of_file -> ()
