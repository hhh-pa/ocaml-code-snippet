let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let s = Array.make n 0L in
    let f = Array.make n 0L in
    for i = 0 to n - 1 do
      s.(i) <- Scanf.scanf " %Ld" (fun x -> x);
      f.(i) <- Scanf.scanf " %Ld" (fun x -> x)
    done;
    let o = Array.init n (fun i -> i) in
    Array.sort (fun a b ->
      if f.(a) <> f.(b) then
        if f.(a) < f.(b) then -1 else 1
      else if s.(a) <> s.(b) then
        if s.(a) < s.(b) then -1 else 1
      else
        compare a b
    ) o;
    let sel = ref [] in
    let last = ref (-1L) in
    let first = ref true in
    for idx = 0 to n - 1 do
      let j = o.(idx) in
      if !first || s.(j) >= !last then begin
        sel := j :: !sel;
        last := f.(j);
        first := false
      end
    done;
    let sel_list = List.rev !sel in
    Printf.printf "%d\n" (List.length sel_list);
    let p_first = ref true in
    List.iter (fun x ->
      if not !p_first then print_string " ";
      p_first := false;
      print_int x
    ) sel_list;
    print_newline ()
  with End_of_file -> ()
