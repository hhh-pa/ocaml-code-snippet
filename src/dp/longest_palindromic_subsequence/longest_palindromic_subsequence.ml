let () =
  try
    let s = Scanf.scanf " %s" (fun str -> str) in
    let n = String.length s in
    if n = 0 then
      Printf.printf "0\n"
    else
      let dp = Array.make_matrix n n 0 in
      for i = 0 to n - 1 do
        dp.(i).(i) <- 1
      done;
      for len = 2 to n do
        for i = 0 to n - len do
          let j = i + len - 1 in
          if s.[i] = s.[j] then
            dp.(i).(j) <- (if len > 2 then dp.(i + 1).(j - 1) else 0) + 2
          else
            dp.(i).(j) <- max dp.(i + 1).(j) dp.(i).(j - 1)
        done
      done;
      Printf.printf "%d\n" dp.(0).(n - 1)
  with End_of_file -> ()
