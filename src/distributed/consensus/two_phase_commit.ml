let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let vote = Array.init n (fun _ -> Scanf.scanf " %s" (fun x -> x)) in
    let all_yes = ref true in
    for i = 0 to n - 1 do
      if vote.(i) <> "YES" then all_yes := false
    done;
    let decision = if !all_yes then "COMMIT" else "ABORT" in
    Printf.printf "DECISION: %s\n" decision;
    for i = 0 to n - 1 do
      let state = if vote.(i) = "TIMEOUT" then "ABORT" else decision in
      Printf.printf "P%d: %s\n" i state
    done
  with End_of_file -> ()
