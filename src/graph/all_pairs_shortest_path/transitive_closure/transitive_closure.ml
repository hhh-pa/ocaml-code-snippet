let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let m = Scanf.scanf " %d" (fun x -> x) in
    let t = Array.init n (fun i ->
      Array.init n (fun j -> if i = j then 1 else 0)
    ) in
    for _ = 0 to m - 1 do
      let u = Scanf.scanf " %d" (fun x -> x) in
      let v = Scanf.scanf " %d" (fun x -> x) in
      t.(u).(v) <- 1
    done;
    for k = 0 to n - 1 do
      let tk = t.(k) in
      for i = 0 to n - 1 do
        if t.(i).(k) = 1 then
          let ti = t.(i) in
          for j = 0 to n - 1 do
            if tk.(j) = 1 then ti.(j) <- 1
          done
      done
    done;
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        if j > 0 then Printf.printf " ";
        Printf.printf "%d" t.(i).(j)
      done;
      Printf.printf "\n"
    done
  with End_of_file -> ()
