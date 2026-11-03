let heap_sort a =
  let n = Array.length a in
  let sift_down start finish =
    let root = ref start in
    let cont = ref true in
    while 2 * !root + 1 <= finish && !cont do
      let child = 2 * !root + 1 in
      let sw = ref !root in
      if a.(!sw) < a.(child) then sw := child;
      if child + 1 <= finish && a.(!sw) < a.(child + 1) then sw := child + 1;
      if !sw = !root then cont := false
      else begin
        let tmp = a.(!root) in
        a.(!root) <- a.(!sw);
        a.(!sw) <- tmp;
        root := !sw
      end
    done
  in
  for start = n / 2 - 1 downto 0 do
    sift_down start (n - 1)
  done;
  for finish = n - 1 downto 1 do
    let tmp = a.(0) in
    a.(0) <- a.(finish);
    a.(finish) <- tmp;
    sift_down 0 (finish - 1)
  done

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let nums = Array.init n (fun _ -> Scanf.scanf " %d" (fun x -> x)) in
    heap_sort nums;
    print_string "[";
    for i = 0 to n - 1 do
      if i > 0 then print_string ",";
      print_int nums.(i)
    done;
    print_string "]"
  with End_of_file -> ()
