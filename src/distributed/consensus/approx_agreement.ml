let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let f = Scanf.scanf " %d" (fun x -> x) in
    let rounds = Scanf.scanf " %d" (fun x -> x) in
    let vals = Array.init n (fun _ -> Scanf.scanf " %f" (fun x -> x)) in
    for _ = 0 to rounds - 1 do
      let recvd = Array.copy vals in
      Array.sort Float.compare recvd;
      let lo = f in
      let hi = n - f in
      if hi > lo then begin
        let s = ref 0.0 in
        for k = lo to hi - 1 do
          s := !s +. recvd.(k)
        done;
        let avg = !s /. float_of_int (hi - lo) in
        for i = 0 to n - 1 do
          vals.(i) <- avg
        done
      end
    done;
    for i = 0 to n - 1 do
      Printf.printf "P%d: %.6f\n" i vals.(i)
    done
  with End_of_file -> ()
