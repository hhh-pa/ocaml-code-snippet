let () =
  try
    let x0 = Scanf.scanf " %Ld" (fun x -> x) in
    let m = Scanf.scanf " %Ld" (fun x -> x) in
    let cnt = Scanf.scanf " %Ld" (fun x -> x) in
    let x = ref x0 in
    let buf = Buffer.create (Int64.to_int cnt) in
    let i = ref 0L in
    while !i < cnt do
      x := Int64.rem (Int64.mul !x !x) m;
      let bit = if Int64.logand !x 1L = 1L then '1' else '0' in
      Buffer.add_char buf bit;
      i := Int64.add !i 1L
    done;
    Printf.printf "%s\n" (Buffer.contents buf);
    Printf.printf "%Ld\n" !x
  with End_of_file -> ()
