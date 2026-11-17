let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let arr = Array.init (max n 0) (fun _ -> Scanf.scanf " %d" (fun x -> x)) in
    let target = Scanf.scanf " %d" (fun x -> x) in
    let ans = ref (-1) in
    (try
      for i = 0 to Array.length arr - 1 do
        if arr.(i) = target then (ans := i; raise Exit)
      done
    with Exit -> ());
    print_int !ans
  with End_of_file -> ()
