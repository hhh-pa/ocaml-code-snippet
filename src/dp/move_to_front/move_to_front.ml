let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let m = Scanf.scanf " %d" (fun x -> x) in
    let lst = ref (List.init n (fun i -> i)) in
    let tot = ref 0L in
    for _ = 1 to m do
      let x = Scanf.scanf " %d" (fun v -> v) in
      let rec remove_and_find idx = function
        | [] -> (0, [])
        | h :: t when h = x -> (idx, t)
        | h :: t ->
          let (found_idx, rest) = remove_and_find (idx + 1) t in
          (found_idx, h :: rest)
      in
      let (pos, rest) = remove_and_find 0 !lst in
      tot := Int64.add !tot (Int64.of_int (pos + 1));
      lst := x :: rest
    done;
    Printf.printf "%Ld\n" !tot
  with End_of_file -> ()
