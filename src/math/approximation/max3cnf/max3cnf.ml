let expected_of cls asg =
  let tot = ref 0.0 in
  Array.iter (fun cl ->
    let sat = ref false in
    let open_count = ref 0 in
    let i = ref 0 in
    while !i < 3 && not !sat do
      let lit = cl.(!i) in
      let v = abs lit in
      if asg.(v) >= 0 then begin
        let lit_val = if lit > 0 then 1 else 0 in
        if lit_val = asg.(v) then sat := true
      end else
        incr open_count;
      incr i
    done;
    if !sat then
      tot := !tot +. 1.0
    else if !open_count > 0 then
      let p = 1.0 /. (float_of_int (1 lsl !open_count)) in
      tot := !tot +. (1.0 -. p)
  ) cls;
  !tot

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let m = Scanf.scanf " %d" (fun x -> x) in
    let cls = Array.init m (fun _ ->
      let a = Scanf.scanf " %d" (fun x -> x) in
      let b = Scanf.scanf " %d" (fun x -> x) in
      let c = Scanf.scanf " %d" (fun x -> x) in
      [| a; b; c |]
    ) in
    let asg = Array.make (n + 1) (-1) in
    for v = 1 to n do
      asg.(v) <- 1;
      let e1 = expected_of cls asg in
      asg.(v) <- 0;
      let e0 = expected_of cls asg in
      asg.(v) <- (if e1 >= e0 then 1 else 0)
    done;
    let sat = ref 0 in
    Array.iter (fun cl ->
      let s = ref false in
      for i = 0 to 2 do
        let lit = cl.(i) in
        let lit_val = if lit > 0 then 1 else 0 in
        if lit_val = asg.(abs lit) then s := true
      done;
      if !s then incr sat
    ) cls;
    for v = 1 to n do
      if v > 1 then print_string " ";
      print_int asg.(v)
    done;
    print_newline ();
    Printf.printf "%d\n" !sat
  with End_of_file -> ()
