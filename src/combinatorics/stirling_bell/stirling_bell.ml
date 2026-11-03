let () =
  try
    let mode = Scanf.scanf " %s" (fun s -> s) in
    if mode = "S2" then begin
      let n = Scanf.scanf " %d" (fun x -> x) in
      let k = Scanf.scanf " %d" (fun x -> x) in
      let s = Array.make_matrix (n + 1) (k + 1) 0L in
      s.(0).(0) <- 1L;
      for i = 1 to n do
        for j = 1 to min i k do
          let term = Int64.mul (Int64.of_int j) s.(i - 1).(j) in
          s.(i).(j) <- Int64.add term s.(i - 1).(j - 1)
        done
      done;
      Printf.printf "%Ld\n" s.(n).(k)
    end else if mode = "S1" then begin
      let n = Scanf.scanf " %d" (fun x -> x) in
      let k = Scanf.scanf " %d" (fun x -> x) in
      let s = Array.make_matrix (n + 1) (k + 1) 0L in
      s.(0).(0) <- 1L;
      for i = 1 to n do
        for j = 1 to min i k do
          let term = Int64.mul (Int64.of_int (-(i - 1))) s.(i - 1).(j) in
          s.(i).(j) <- Int64.add term s.(i - 1).(j - 1)
        done
      done;
      Printf.printf "%Ld\n" s.(n).(k)
    end else begin
      let n = Scanf.scanf " %d" (fun x -> x) in
      let s = Array.make_matrix (n + 1) (n + 1) 0L in
      s.(0).(0) <- 1L;
      for i = 1 to n do
        for j = 1 to i do
          let term = Int64.mul (Int64.of_int j) s.(i - 1).(j) in
          s.(i).(j) <- Int64.add term s.(i - 1).(j - 1)
        done
      done;
      let b = ref 0L in
      for j = 0 to n do
        b := Int64.add !b s.(n).(j)
      done;
      Printf.printf "%Ld\n" !b
    end
  with End_of_file -> ()
