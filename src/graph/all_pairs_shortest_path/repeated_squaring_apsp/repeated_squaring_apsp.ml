let inf = 400_000_000_000_000L

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let l = Array.init n (fun _ ->
      Array.init n (fun _ ->
        let x = Scanf.scanf " %Ld" (fun x -> x) in
        if x = 999999L then inf else x
      )
    ) in
    let l_ref = ref l in
    let m = ref 1 in
    while !m < n - 1 do
      let curr = !l_ref in
      let n_mat = Array.init n (fun i ->
        let curr_i = curr.(i) in
        Array.init n (fun j ->
          let b = ref inf in
          for k = 0 to n - 1 do
            let v = Int64.add curr_i.(k) curr.(k).(j) in
            if v < !b then b := v
          done;
          !b
        )
      ) in
      l_ref := n_mat;
      m := !m * 2
    done;
    let res = !l_ref in
    if n = 1 then res.(0).(0) <- 0L;
    let half_inf = Int64.div inf 2L in
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        if j > 0 then Printf.printf " ";
        if res.(i).(j) >= half_inf then
          Printf.printf "INF"
        else
          Printf.printf "%Ld" res.(i).(j)
      done;
      Printf.printf "\n"
    done
  with End_of_file -> ()
