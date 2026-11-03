let radix_sort nums =
  let n = Array.length nums in
  if n > 0 then begin
    let m = ref nums.(0) in
    for i = 1 to n - 1 do
      if nums.(i) > !m then m := nums.(i)
    done;
    let exp = ref 1 in
    while !exp <= !m do
      let count = Array.make 10 [] in
      for i = 0 to n - 1 do
        let digit = (nums.(i) / !exp) mod 10 in
        count.(digit) <- nums.(i) :: count.(digit)
      done;
      let k = ref 0 in
      for d = 0 to 9 do
        let l = List.rev count.(d) in
        List.iter (fun v -> nums.(!k) <- v; incr k) l
      done;
      exp := !exp * 10
    done
  end

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let nums = Array.init n (fun _ -> Scanf.scanf " %d" (fun x -> x)) in
    radix_sort nums;
    print_string "[";
    for i = 0 to n - 1 do
      if i > 0 then print_string ",";
      print_int nums.(i)
    done;
    print_string "]"
  with End_of_file -> ()
