let insertion_sort a =
  let n = Array.length a in
  for i = 1 to n - 1 do
    let cur = a.(i) in
    let j = ref (i - 1) in
    while !j >= 0 && a.(!j) > cur do
      a.(!j + 1) <- a.(!j);
      decr j
    done;
    a.(!j + 1) <- cur
  done

let bucket_sort nums k =
  let n = Array.length nums in
  if n > 0 then begin
    let min_v = ref nums.(0) in
    let max_v = ref nums.(0) in
    for i = 1 to n - 1 do
      if nums.(i) < !min_v then min_v := nums.(i);
      if nums.(i) > !max_v then max_v := nums.(i)
    done;
    if !max_v <> !min_v then begin
      let diff = !max_v - !min_v in
      let buckets = Array.make (k + 1) [] in
      for i = 0 to n - 1 do
        let v = nums.(i) in
        let idx = (v - !min_v) * k / diff in
        buckets.(idx) <- v :: buckets.(idx)
      done;
      let pos = ref 0 in
      for b = 0 to k do
        let arr = Array.of_list (List.rev buckets.(b)) in
        insertion_sort arr;
        for i = 0 to Array.length arr - 1 do
          nums.(!pos) <- arr.(i);
          incr pos
        done
      done
    end
  end

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let nums = Array.init n (fun _ -> Scanf.scanf " %d" (fun x -> x)) in
    bucket_sort nums 8;
    print_string "[";
    for i = 0 to n - 1 do
      if i > 0 then print_string ",";
      print_int nums.(i)
    done;
    print_string "]"
  with End_of_file -> ()
