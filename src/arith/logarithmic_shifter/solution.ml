let () =
  try
    let data = Scanf.scanf " %s" (fun x -> x) in
    let amt = Scanf.scanf " %s" (fun x -> x) in
    let n = String.length data in
    let w = String.length amt in
    let cur = ref (Array.init n (fun i -> data.[i])) in
    let step = ref 1 in
    for j = 0 to w - 1 do
      if amt.[w - 1 - j] = '1' then begin
        let sign = !cur.(0) in
        let nxt = Array.make n sign in
        if !step < n then
          for i = !step to n - 1 do
            nxt.(i) <- !cur.(i - !step)
          done;
        cur := nxt
      end;
      if !step < n then step := !step * 2
    done;
    Array.iter print_char !cur
  with End_of_file -> ()
