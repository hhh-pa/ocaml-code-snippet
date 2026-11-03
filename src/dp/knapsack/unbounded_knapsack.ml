let () =
  try
    let t = Scanf.scanf " %d" (fun x -> x) in
    for _ = 1 to t do
      let n = Scanf.scanf " %d" (fun x -> x) in
      let s = Scanf.scanf " %d" (fun x -> x) in
      let w = Array.init n (fun _ -> Scanf.scanf " %d" (fun x -> x)) in
      let dp = Array.make (s + 1) 0 in
      for i = 0 to s do
        let cur = ref dp.(i) in
        for j = 0 to n - 1 do
          let weight = w.(j) in
          if weight <= i then
            let cand = dp.(i - weight) + weight in
            if cand > !cur then cur := cand
        done;
        dp.(i) <- !cur
      done;
      Printf.printf "%d\n" dp.(s)
    done
  with End_of_file -> ()
