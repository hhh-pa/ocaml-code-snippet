let is_prime n =
  if n < 2 then false
  else begin
    let res = ref true in
    let d = ref 2 in
    while !d * !d <= n do
      if n mod !d = 0 then res := false;
      incr d
    done;
    !res
  end

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    if n = 6 then
      print_endline "NONE"
    else if is_prime n then begin
      for i = 0 to n - 1 do
        for j = 0 to n - 1 do
          if j > 0 then print_char ' ';
          print_int ((i + j) mod n)
        done;
        print_newline ()
      done;
      for i = 0 to n - 1 do
        for j = 0 to n - 1 do
          if j > 0 then print_char ' ';
          print_int ((i + 2 * j) mod n)
        done;
        print_newline ()
      done;
      print_endline "ORTHOGONAL"
    end else begin
      for i = 0 to n - 1 do
        for j = 0 to n - 1 do
          if j > 0 then print_char ' ';
          print_int ((i + j) mod n)
        done;
        print_newline ()
      done;
      print_endline "NO_PAIR"
    end
  with End_of_file -> ()
