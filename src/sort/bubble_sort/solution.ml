let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let a = Array.init n (fun _ -> Scanf.scanf " %Ld" (fun x -> x)) in
    let swaps = ref 0L in
    let stop = ref false in
    let i = ref 0 in
    while !i < n - 1 && not !stop do
      let swapped = ref false in
      for j = 0 to n - 2 - !i do
        if a.(j) > a.(j + 1) then begin
          let tmp = a.(j) in
          a.(j) <- a.(j + 1);
          a.(j + 1) <- tmp;
          swaps := Int64.add !swaps 1L;
          swapped := true
        end
      done;
      if not !swapped then stop := true;
      incr i
    done;
    for k = 0 to n - 1 do
      if k > 0 then print_string " ";
      Printf.printf "%Ld" a.(k)
    done;
    Printf.printf "\n%Ld\n" !swaps
  with End_of_file -> ()
