let shell_sort a =
  let n = Array.length a in
  let gap = ref (n / 2) in
  while !gap > 0 do
    for i = !gap to n - 1 do
      let tmp = a.(i) in
      let j = ref i in
      while !j >= !gap && a.(!j - !gap) > tmp do
        a.(!j) <- a.(!j - !gap);
        j := !j - !gap
      done;
      a.(!j) <- tmp
    done;
    gap := !gap / 2
  done

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let nums = Array.init n (fun _ -> Scanf.scanf " %d" (fun x -> x)) in
    shell_sort nums;
    print_string "[";
    for i = 0 to n - 1 do
      if i > 0 then print_string ",";
      print_int nums.(i)
    done;
    print_string "]"
  with End_of_file -> ()
