let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let d = Array.init (n + 1) (fun _ -> Scanf.scanf " %Ld" (fun x -> x)) in
    if n <= 1 then
      Printf.printf "0\n"
    else
      let m = Array.make_matrix n n 0L in
      for l = 2 to n do
        for i = 0 to n - l do
          let j = i + l - 1 in
          m.(i).(j) <- Int64.max_int;
          let di = d.(i) in
          let dj1 = d.(j + 1) in
          for k = i to j - 1 do
            let cost =
              Int64.add (Int64.add m.(i).(k) m.(k + 1).(j))
                (Int64.mul (Int64.mul di d.(k + 1)) dj1)
            in
            if cost < m.(i).(j) then m.(i).(j) <- cost
          done
        done
      done;
      Printf.printf "%Ld\n" m.(0).(n - 1)
  with End_of_file -> ()
