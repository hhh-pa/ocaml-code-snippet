let rec sort3 a lo hi =
  if lo < hi then begin
    let pivot = a.(lo + (hi - lo) / 2) in
    let lt = ref lo in
    let gt = ref hi in
    let i = ref lo in
    while !i <= !gt do
      if a.(!i) < pivot then begin
        let tmp = a.(!lt) in
        a.(!lt) <- a.(!i);
        a.(!i) <- tmp;
        incr lt;
        incr i
      end else if a.(!i) > pivot then begin
        let tmp = a.(!i) in
        a.(!i) <- a.(!gt);
        a.(!gt) <- tmp;
        decr gt
      end else
        incr i
    done;
    sort3 a lo (!lt - 1);
    sort3 a (!gt + 1) hi
  end

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let nums = Array.init n (fun _ -> Scanf.scanf " %d" (fun x -> x)) in
    if n > 0 then sort3 nums 0 (n - 1);
    print_string "[";
    for i = 0 to n - 1 do
      if i > 0 then print_string ",";
      print_int nums.(i)
    done;
    print_string "]"
  with End_of_file -> ()
