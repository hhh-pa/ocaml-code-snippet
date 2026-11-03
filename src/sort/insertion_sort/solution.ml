let insertion_sort nums =
  let n = Array.length nums in
  for i = 1 to n - 1 do
    let cur = nums.(i) in
    let j = ref (i - 1) in
    while !j >= 0 && nums.(!j) > cur do
      nums.(!j + 1) <- nums.(!j);
      decr j
    done;
    nums.(!j + 1) <- cur
  done

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let nums = Array.init n (fun _ -> Scanf.scanf " %d" (fun x -> x)) in
    insertion_sort nums;
    print_string "[";
    for i = 0 to n - 1 do
      if i > 0 then print_string ",";
      print_int nums.(i)
    done;
    print_string "]"
  with End_of_file -> ()
