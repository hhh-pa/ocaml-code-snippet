let merge_sort nums =
  let n = Array.length nums in
  if n > 1 then begin
    let aux = Array.make n 0 in
    let rec sort_rec l r =
      if l < r then begin
        let m = (l + r) / 2 in
        sort_rec l m;
        sort_rec (m + 1) r;
        let i = ref l in
        let j = ref (m + 1) in
        let k = ref l in
        while !i <= m && !j <= r do
          if nums.(!i) <= nums.(!j) then begin
            aux.(!k) <- nums.(!i);
            incr i
          end else begin
            aux.(!k) <- nums.(!j);
            incr j
          end;
          incr k
        done;
        while !i <= m do
          aux.(!k) <- nums.(!i);
          incr i;
          incr k
        done;
        while !j <= r do
          aux.(!k) <- nums.(!j);
          incr j;
          incr k
        done;
        for idx = l to r do
          nums.(idx) <- aux.(idx)
        done
      end
    in
    sort_rec 0 (n - 1)
  end

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let nums = Array.init n (fun _ -> Scanf.scanf " %d" (fun x -> x)) in
    merge_sort nums;
    print_string "[";
    for i = 0 to n - 1 do
      if i > 0 then print_string ",";
      print_int nums.(i)
    done;
    print_string "]"
  with End_of_file -> ()
