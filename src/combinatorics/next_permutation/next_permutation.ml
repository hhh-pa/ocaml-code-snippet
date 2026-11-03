let () =
  try
    let mode = Scanf.scanf " %s" (fun s -> s) in
    if mode = "P" then begin
      let n = Scanf.scanf " %d" (fun x -> x) in
      let p = Array.init n (fun _ -> Scanf.scanf " %d" (fun x -> x)) in
      let i = ref (n - 2) in
      while !i >= 0 && p.(!i) >= p.(!i + 1) do
        decr i
      done;
      if !i < 0 then
        print_endline "NONE"
      else begin
        let j = ref (n - 1) in
        while p.(!j) <= p.(!i) do
          decr j
        done;
        let tmp = p.(!i) in
        p.(!i) <- p.(!j);
        p.(!j) <- tmp;
        let l = ref (!i + 1) in
        let r = ref (n - 1) in
        while !l < !r do
          let t = p.(!l) in
          p.(!l) <- p.(!r);
          p.(!r) <- t;
          incr l;
          decr r
        done;
        for k = 0 to n - 1 do
          if k > 0 then print_char ' ';
          print_int p.(k)
        done;
        print_newline ()
      end
    end else begin
      let n = Scanf.scanf " %d" (fun x -> x) in
      let r = Scanf.scanf " %d" (fun x -> x) in
      let c = Array.init r (fun _ -> Scanf.scanf " %d" (fun x -> x)) in
      let i = ref (r - 1) in
      while !i >= 0 && c.(!i) = n - r + !i do
        decr i
      done;
      if !i < 0 then
        print_endline "NONE"
      else begin
        c.(!i) <- c.(!i) + 1;
        for j = !i + 1 to r - 1 do
          c.(j) <- c.(j - 1) + 1
        done;
        for k = 0 to r - 1 do
          if k > 0 then print_char ' ';
          print_int c.(k)
        done;
        print_newline ()
      end
    end
  with End_of_file -> ()
