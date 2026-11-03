let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let p = Array.init n (fun i -> i) in
    let d = Array.make n (-1) in
    let print_p () =
      for i = 0 to n - 1 do
        if i > 0 then print_char ' ';
        print_int p.(i)
      done;
      print_newline ()
    in
    print_p ();
    let rec loop () =
      let mob = ref (-1) in
      let mobpos = ref (-1) in
      for i = 0 to n - 1 do
        let v = p.(i) in
        let j = i + d.(v) in
        if j >= 0 && j < n && p.(j) < v && v > !mob then begin
          mob := v;
          mobpos := i
        end
      done;
      if !mob >= 0 then begin
        let j = !mobpos + d.(!mob) in
        let tmp = p.(!mobpos) in
        p.(!mobpos) <- p.(j);
        p.(j) <- tmp;
        for v = 0 to n - 1 do
          if v > !mob then d.(v) <- -d.(v)
        done;
        print_p ();
        loop ()
      end
    in
    loop ()
  with End_of_file -> ()
