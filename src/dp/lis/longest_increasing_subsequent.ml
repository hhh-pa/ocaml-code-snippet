let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let tails = Array.make n 0 in
    let len = ref 0 in
    for _ = 0 to n - 1 do
      let x = Scanf.scanf " %d" (fun v -> v) in
      let l = ref 0 in
      let r = ref !len in
      while !l < !r do
        let mid = (!l + !r) / 2 in
        if tails.(mid) < x then
          l := mid + 1
        else
          r := mid
      done;
      let it = !l in
      tails.(it) <- x;
      if it = !len then incr len
    done;
    Printf.printf "%d\n" !len
  with End_of_file -> ()
