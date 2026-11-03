let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    if n <= 0 then exit 0;
    let a = Array.make_matrix n n 0.0 in
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        a.(i).(j) <- Scanf.scanf " %f" (fun x -> x)
      done
    done;

    let l = Array.make_matrix n n 0.0 in
    for i = 0 to n - 1 do
      for j = 0 to i do
        let s = ref a.(i).(j) in
        for k = 0 to j - 1 do
          s := !s -. l.(i).(k) *. l.(j).(k)
        done;
        if i = j then begin
          if !s <= 1e-9 then begin
            Printf.printf "NOT_POSITIVE_DEFINITE\n";
            exit 0
          end;
          l.(i).(j) <- sqrt !s
        end else begin
          l.(i).(j) <- !s /. l.(j).(j)
        end
      done
    done;

    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        if j > 0 then print_string " ";
        Printf.printf "%.6f" l.(i).(j)
      done;
      print_newline ()
    done
  with End_of_file -> ()
