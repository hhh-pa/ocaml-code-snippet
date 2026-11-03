let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    if n = 0 then
      Printf.printf "0\n"
    else
      let v = Array.init n (fun _ -> Scanf.scanf " %Ld" (fun x -> x)) in
      let dp = Array.make_matrix n n 0L in
      for i = 0 to n - 1 do
        dp.(i).(i) <- v.(i)
      done;
      for l = 2 to n do
        for i = 0 to n - l do
          let j = i + l - 1 in
          let a = if i + 2 <= j then dp.(i + 2).(j) else 0L in
          let b = if i + 1 <= j - 1 then dp.(i + 1).(j - 1) else 0L in
          let c = if i <= j - 2 then dp.(i).(j - 2) else 0L in
          let choice1 = Int64.add v.(i) (min a b) in
          let choice2 = Int64.add v.(j) (min b c) in
          dp.(i).(j) <- max choice1 choice2
        done
      done;
      Printf.printf "%Ld\n" dp.(0).(n - 1)
  with End_of_file -> ()
