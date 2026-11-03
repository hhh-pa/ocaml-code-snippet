let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let a = Array.make_matrix n n 0L in
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        a.(i).(j) <- Scanf.scanf " %Ld" (fun x -> x)
      done
    done;
    let b = Array.make_matrix n n 0L in
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        b.(i).(j) <- Scanf.scanf " %Ld" (fun x -> x)
      done
    done;
    let c = Array.make_matrix n n 0L in
    for i = 0 to n - 1 do
      for k = 0 to n - 1 do
        let aik = a.(i).(k) in
        for j = 0 to n - 1 do
          c.(i).(j) <- Int64.add c.(i).(j) (Int64.mul aik b.(k).(j))
        done
      done
    done;
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        if j > 0 then print_string " ";
        Printf.printf "%Ld" c.(i).(j)
      done;
      print_newline ()
    done
  with End_of_file -> ()
