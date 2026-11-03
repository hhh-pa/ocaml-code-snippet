let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let d = Array.make n 0 in
    let w = Array.make n 0L in
    let mx = ref 0 in
    for i = 0 to n - 1 do
      d.(i) <- Scanf.scanf " %d" (fun v -> v);
      w.(i) <- Scanf.scanf " %Ld" (fun v -> v);
      if d.(i) > !mx then mx := d.(i)
    done;
    let order = Array.init n (fun i -> i) in
    Array.sort (fun a b ->
      if w.(a) <> w.(b) then
        if w.(a) > w.(b) then -1 else 1
      else
        compare a b
    ) order;
    let slot = Array.make (!mx + 1) (-1) in
    let ins = Array.make n false in
    for idx = 0 to n - 1 do
      let i = order.(idx) in
      let t = ref (min d.(i) !mx) in
      while !t >= 1 && slot.(!t) <> -1 do
        decr t
      done;
      if !t >= 1 then begin
        slot.(!t) <- i;
        ins.(i) <- true
      end
    done;
    let pen = ref 0L in
    let sel = ref [] in
    for i = 0 to n - 1 do
      if ins.(i) then
        sel := i :: !sel
      else
        pen := Int64.add !pen w.(i)
    done;
    let sel_list = List.rev !sel in
    Printf.printf "%Ld\n" !pen;
    let first = ref true in
    List.iter (fun x ->
      if not !first then print_string " ";
      first := false;
      print_int x
    ) sel_list;
    print_newline ()
  with End_of_file -> ()
