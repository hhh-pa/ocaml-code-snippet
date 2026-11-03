let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let m = Scanf.scanf " %d" (fun x -> x) in
    let cs = Array.init m (fun _ ->
      let i = Scanf.scanf " %d" (fun x -> x) in
      let j = Scanf.scanf " %d" (fun x -> x) in
      let b = Scanf.scanf " %Ld" (fun x -> x) in
      (i, j, b)
    ) in
    let d = Array.make n 0L in
    for _ = 0 to n - 1 do
      Array.iter (fun (i, j, b) ->
        let cand = Int64.add d.(i) b in
        if cand < d.(j) then d.(j) <- cand
      ) cs
    done;
    let infeasible = ref false in
    Array.iter (fun (i, j, b) ->
      if Int64.add d.(i) b < d.(j) then
        infeasible := true
    ) cs;
    if !infeasible then
      Printf.printf "INFEASIBLE\n"
    else begin
      for i = 0 to n - 1 do
        if i > 0 then Printf.printf " ";
        Printf.printf "%Ld" d.(i)
      done;
      Printf.printf "\n"
    end
  with End_of_file -> ()
