let selection_sort (a : int64 array) =
  let n = Array.length a in
  for i = 0 to n - 1 do
    let min_idx = ref i in
    for j = i + 1 to n - 1 do
      if a.(j) < a.(!min_idx) then min_idx := j
    done;
    if !min_idx <> i then begin
      let tmp = a.(i) in
      a.(i) <- a.(!min_idx);
      a.(!min_idx) <- tmp
    end
  done

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let a = Array.init n (fun _ -> Scanf.scanf " %Ld" (fun x -> x)) in
    selection_sort a;
    for i = 0 to n - 1 do
      if i > 0 then print_string " ";
      Printf.printf "%Ld" a.(i)
    done;
    print_newline ()
  with End_of_file -> ()
