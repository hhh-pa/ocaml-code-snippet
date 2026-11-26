let () =
  try
    let a = ref (Scanf.scanf " %Ld" (fun x -> x)) in
    let n = ref (Scanf.scanf " %Ld" (fun x -> x)) in
    a := Int64.rem !a !n;
    let t = ref 1L in
    while !a <> 0L do
      while Int64.rem !a 2L = 0L do
        a := Int64.div !a 2L;
        let r = Int64.rem !n 8L in
        if r = 3L || r = 5L then t := Int64.neg !t
      done;
      let tmp = !a in
      a := !n;
      n := tmp;
      if Int64.rem !a 4L = 3L && Int64.rem !n 4L = 3L then t := Int64.neg !t;
      a := Int64.rem !a !n
    done;
    if !n = 1L then print_string (Int64.to_string !t) else print_string "0"
  with End_of_file -> ()
