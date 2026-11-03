let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let m = Scanf.scanf " %d" (fun x -> x) in
    let a = Array.init n (fun _ -> Scanf.scanf " %d" (fun x -> x)) in
    let b = Array.init m (fun _ -> Scanf.scanf " %d" (fun x -> x)) in
    let l_mat = Array.make_matrix (m + 1) (n + 1) 0 in
    for i = 1 to m do
      for j = 1 to n do
        if b.(i - 1) = a.(j - 1) then
          l_mat.(i).(j) <- l_mat.(i - 1).(j - 1) + 1
        else
          l_mat.(i).(j) <- max l_mat.(i - 1).(j) l_mat.(i).(j - 1)
      done
    done;
    let index = l_mat.(m).(n) in
    let ans = Array.make index 0 in
    let i = ref m in
    let j = ref n in
    let idx = ref index in
    while !i > 0 && !j > 0 do
      if b.(!i - 1) = a.(!j - 1) then begin
        ans.(!idx - 1) <- b.(!i - 1);
        decr i;
        decr j;
        decr idx
      end else if l_mat.(!i - 1).(!j) > l_mat.(!i).(!j - 1) then
        decr i
      else
        decr j
    done;
    for k = 0 to index - 1 do
      if k > 0 then print_string " ";
      print_int ans.(k)
    done;
    print_newline ()
  with End_of_file -> ()
