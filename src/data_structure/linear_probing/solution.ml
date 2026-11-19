let () =
  try
    let m = Scanf.scanf " %d" (fun x -> x) in
    let n = Scanf.scanf " %d" (fun x -> x) in
    if m > 0 && n >= 0 then begin
      let table = Array.make m None in
      for _ = 1 to n do
        let k = Scanf.scanf " %d" (fun x -> x) in
        let h = ((k mod m) + m) mod m in
        let placed = ref false in
        for t = 0 to m - 1 do
          if not !placed then
            let i = (h + t) mod m in
            match table.(i) with
            | None ->
                table.(i) <- Some k;
                placed := true
            | Some _ -> ()
        done
      done;
      for i = 0 to m - 1 do
        if i > 0 then print_string " ";
        match table.(i) with None -> print_string "_" | Some v -> print_int v
      done
    end
  with End_of_file -> ()
