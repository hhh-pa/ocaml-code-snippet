let inf = 999999

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let m = Scanf.scanf " %d" (fun x -> x) in
    let d = Array.init (n + 1) (fun i ->
      Array.init (n + 1) (fun j -> if i = j then 0 else inf)
    ) in
    for _ = 0 to m - 1 do
      let u = Scanf.scanf " %d" (fun x -> x) in
      let v = Scanf.scanf " %d" (fun x -> x) in
      let wt = Scanf.scanf " %d" (fun x -> x) in
      d.(u).(v) <- wt
    done;
    for k = 1 to n do
      for i = 1 to n do
        if d.(i).(k) < inf then
          for j = 1 to n do
            let cand = d.(i).(k) + d.(k).(j) in
            if cand < d.(i).(j) then
              d.(i).(j) <- cand
          done
      done
    done;
    let q = Scanf.scanf " %d" (fun x -> x) in
    for _ = 0 to q - 1 do
      let a = Scanf.scanf " %d" (fun x -> x) in
      let b = Scanf.scanf " %d" (fun x -> x) in
      let ans = d.(a).(b) in
      if ans >= inf then
        Printf.printf "-1\n"
      else
        Printf.printf "%d\n" ans
    done
  with End_of_file -> ()
