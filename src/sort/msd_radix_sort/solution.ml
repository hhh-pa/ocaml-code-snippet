let clean_line s =
  let len = String.length s in
  if len > 0 && s.[len - 1] = '\r' then String.sub s 0 (len - 1) else s

let () =
  try
    let line1 = input_line stdin in
    let n = int_of_string (String.trim line1) in
    let a = Array.init n (fun _ -> clean_line (input_line stdin)) in
    Array.sort String.compare a;
    print_string "[";
    for i = 0 to n - 1 do
      if i > 0 then print_string ",";
      print_string a.(i)
    done;
    print_string "]"
  with End_of_file -> ()
