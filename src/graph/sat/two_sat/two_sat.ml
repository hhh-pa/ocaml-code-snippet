let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let m = Scanf.scanf " %d" (fun x -> x) in
    let total_n = 2 * n in
    let adj = Array.make total_n [] in
    let lit k =
      let v = abs k - 1 in
      if k > 0 then 2 * v + 1 else 2 * v
    in
    for _ = 1 to m do
      let a = Scanf.scanf " %d" (fun x -> x) in
      let b = Scanf.scanf " %d" (fun x -> x) in
      let not_a = lit (-a) in
      let not_b = lit (-b) in
      let lit_a = lit a in
      let lit_b = lit b in
      adj.(not_a) <- lit_b :: adj.(not_a);
      adj.(not_b) <- lit_a :: adj.(not_b)
    done;

    let idx = Array.make total_n 0 in
    let low = Array.make total_n 0 in
    let on = Array.make total_n false in
    let comp = Array.make total_n (-1) in
    let st = ref [] in
    let c = ref 0 in
    let t = ref 0 in

    let rec dfs v =
      incr t;
      idx.(v) <- !t;
      low.(v) <- !t;
      st := v :: !st;
      on.(v) <- true;
      List.iter (fun w ->
        if idx.(w) = 0 then begin
          dfs w;
          if low.(w) < low.(v) then low.(v) <- low.(w)
        end else if on.(w) then begin
          if idx.(w) < low.(v) then low.(v) <- idx.(w)
        end
      ) adj.(v);
      if low.(v) = idx.(v) then begin
        let rec pop_comp () =
          match !st with
          | [] -> ()
          | x :: rest ->
            st := rest;
            on.(x) <- false;
            comp.(x) <- !c;
            if x <> v then pop_comp ()
        in
        pop_comp ();
        incr c
      end
    in

    for v = 0 to total_n - 1 do
      if idx.(v) = 0 then dfs v
    done;

    let satisfiable = ref true in
    for i = 0 to n - 1 do
      if comp.(2 * i) = comp.(2 * i + 1) then satisfiable := false
    done;

    if not !satisfiable then
      Printf.printf "UNSATISFIABLE\n"
    else begin
      Printf.printf "SATISFIABLE\n";
      for i = 0 to n - 1 do
        if i > 0 then print_string " ";
        if comp.(2 * i) > comp.(2 * i + 1) then print_string "1"
        else print_string "0"
      done;
      print_newline ()
    end
  with End_of_file -> ()
