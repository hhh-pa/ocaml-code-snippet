let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let _f = Scanf.scanf " %d" (fun x -> x) in
    let commander_val = Scanf.scanf " %d" (fun x -> x) in
    let num_faulty = Scanf.scanf " %d" (fun x -> x) in
    let is_faulty = Array.make n false in
    for _ = 0 to num_faulty - 1 do
      let fid = Scanf.scanf " %d" (fun x -> x) in
      is_faulty.(fid) <- true
    done;
    let decision = if is_faulty.(0) then 0 else commander_val in
    Printf.printf "DECISION: %d\n" decision;
    for i = 1 to n - 1 do
      if not is_faulty.(i) then
        Printf.printf "P%d: %d\n" i decision
    done
  with End_of_file -> ()
