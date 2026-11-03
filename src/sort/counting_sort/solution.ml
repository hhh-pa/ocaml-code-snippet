let counting_sort nums =
  let n = Array.length nums in
  if n > 0 then begin
    let min_v = ref nums.(0) in
    let max_v = ref nums.(0) in
    for i = 1 to n - 1 do
      if nums.(i) < !min_v then min_v := nums.(i);
      if nums.(i) > !max_v then max_v := nums.(i)
    done;
    let range = !max_v - !min_v + 1 in
    let cnt = Array.make range 0 in
    for i = 0 to n - 1 do
      let idx = nums.(i) - !min_v in
      cnt.(idx) <- cnt.(idx) + 1
    done;
    let idx = ref 0 in
    for v = 0 to range - 1 do
      for _ = 1 to cnt.(v) do
        nums.(!idx) <- v + !min_v;
        incr idx
      done
    done
  end

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let nums = Array.init n (fun _ -> Scanf.scanf " %d" (fun x -> x)) in
    counting_sort nums;
    print_string "[";
    for i = 0 to n - 1 do
      if i > 0 then print_string ",";
      print_int nums.(i)
    done;
    print_string "]"
  with End_of_file -> ()
