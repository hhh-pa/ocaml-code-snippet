let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let a = Array.init n (fun _ -> Scanf.scanf " %Ld" (fun x -> x)) in
    let big_n = ref 1 in
    while !big_n < n do
      big_n := !big_n * 2
    done;
    let t = Array.make !big_n 0L in
    for i = 0 to n - 1 do
      t.(i) <- a.(i)
    done;
    let d = ref 1 in
    while !d < !big_n do
      let step = 2 * !d in
      let k = ref 0 in
      while !k < !big_n do
        t.(!k + step - 1) <- Int64.add t.(!k + step - 1) t.(!k + !d - 1);
        k := !k + step
      done;
      d := step
    done;
    t.(!big_n - 1) <- 0L;
    let d2 = ref (!big_n / 2) in
    while !d2 >= 1 do
      let step = 2 * !d2 in
      let k = ref 0 in
      while !k < !big_n do
        let tmp = t.(!k + !d2 - 1) in
        t.(!k + !d2 - 1) <- t.(!k + step - 1);
        t.(!k + step - 1) <- Int64.add t.(!k + step - 1) tmp;
        k := !k + step
      done;
      d2 := !d2 / 2
    done;
    for i = 0 to n - 1 do
      if i > 0 then print_string " ";
      Printf.printf "%Ld" t.(i)
    done;
    print_newline ();
    for i = 0 to n - 1 do
      if i > 0 then print_string " ";
      Printf.printf "%Ld" (Int64.add t.(i) a.(i))
    done;
    print_newline ()
  with End_of_file -> ()
