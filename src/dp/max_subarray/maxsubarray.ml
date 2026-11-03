let () =
  try
    let t = Scanf.scanf " %d" (fun x -> x) in
    for _ = 1 to t do
      let n = Scanf.scanf " %d" (fun x -> x) in
      let a = Array.init n (fun _ -> Scanf.scanf " %d" (fun x -> x)) in
      let max_elem = ref a.(0) in
      for i = 1 to n - 1 do
        if a.(i) > !max_elem then max_elem := a.(i)
      done;
      if !max_elem <= 0 then
        Printf.printf "%d %d\n" !max_elem !max_elem
      else begin
        let max2 = ref 0 in
        for i = 0 to n - 1 do
          if a.(i) > 0 then max2 := !max2 + a.(i)
        done;
        let cur = ref 0 in
        let max1 = ref 0 in
        for i = 0 to n - 1 do
          cur := !cur + a.(i);
          if !cur < 0 then cur := 0
          else if !cur > !max1 then max1 := !cur
        done;
        Printf.printf "%d %d\n" !max1 !max2
      end
    done
  with End_of_file -> ()
