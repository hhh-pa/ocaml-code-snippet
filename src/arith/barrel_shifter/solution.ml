let () =
  try
    let data = Scanf.scanf " %s" (fun x -> x) in
    let hot = Scanf.scanf " %s" (fun x -> x) in
    let n = String.length data in
    let m = String.length hot in
    let k = ref 0 in
    for i = 0 to m - 1 do
      if hot.[m - 1 - i] = '1' then k := i
    done;
    let sign = data.[0] in
    for i = 0 to n - 1 do
      print_char (if i >= !k then data.[i - !k] else sign)
    done
  with End_of_file -> ()
