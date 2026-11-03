let quick_sort a =
  let rec sort_range lo hi =
    if lo < hi then begin
      let pivot = a.((lo + hi) / 2) in
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
      sort_range lo (!lt - 1);
      sort_range (!gt + 1) hi
    end
  in
  let n = Array.length a in
  if n > 1 then sort_range 0 (n - 1)

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let nums = Array.init n (fun _ -> Scanf.scanf " %d" (fun x -> x)) in
    quick_sort nums;
    print_string "[";
    for i = 0 to n - 1 do
      if i > 0 then print_string ",";
      print_int nums.(i)
    done;
    print_string "]"
  with End_of_file -> ()
