let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let m = Scanf.scanf " %d" (fun x -> x) in
    if n >= 0 && m >= 0 then begin
      let adj = Array.make n [] in
      let self_loop = ref false in
      for _ = 1 to m do
        let u = Scanf.scanf " %d" (fun x -> x) in
        let v = Scanf.scanf " %d" (fun x -> x) in
        if u = v then self_loop := true
        else if u >= 0 && u < n && v >= 0 && v < n then begin
          adj.(u) <- v :: adj.(u);
          adj.(v) <- u :: adj.(v)
        end
      done;
      let seen = Array.make n false in
      let rec dfs u parent =
        seen.(u) <- true;
        List.exists
          (fun w -> if not seen.(w) then dfs w u else w <> parent)
          adj.(u)
      in
      let found = ref !self_loop in
      for u = 0 to n - 1 do
        if (not seen.(u)) && dfs u (-1) then found := true
      done;
      print_int (if !found then 1 else 0)
    end
  with End_of_file -> ()
