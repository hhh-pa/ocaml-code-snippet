let () =
  try
    let t = Scanf.scanf " %d" (fun x -> x) in
    for _ = 1 to t do
      let s = Scanf.scanf " %d" (fun x -> x) in
      let n = Scanf.scanf " %d" (fun x -> x) in
      let dp = Array.make (s + 1) 0 in
      for _ = 1 to n do
        let w = Scanf.scanf " %d" (fun x -> x) in
        let v = Scanf.scanf " %d" (fun x -> x) in
        for j = s downto w do
          if dp.(j - w) + v > dp.(j) then
            dp.(j) <- dp.(j - w) + v
        done
      done;
      Printf.printf "%d\n" dp.(s)
    done
  with End_of_file -> ()
