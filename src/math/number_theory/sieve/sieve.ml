let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    if n < 2 then begin
      Printf.printf "0\n\n";
      exit 0
    end;
    let is_prime = Array.make (n + 1) true in
    is_prime.(0) <- false;
    is_prime.(1) <- false;
    let i = ref 2 in
    while !i * !i <= n do
      if is_prime.(!i) then begin
        let j = ref (!i * !i) in
        while !j <= n do
          is_prime.(!j) <- false;
          j := !j + !i
        done
      end;
      incr i
    done;
    let cnt = ref 0 in
    for k = 2 to n do
      if is_prime.(k) then incr cnt
    done;
    Printf.printf "%d\n" !cnt;
    let first = ref true in
    for k = 2 to n do
      if is_prime.(k) then begin
        if not !first then print_string " ";
        print_int k;
        first := false
      end
    done;
    print_newline ()
  with End_of_file -> ()
