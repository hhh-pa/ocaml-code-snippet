let () =
  try
    let mode = Scanf.scanf " %s" (fun s -> s) in
    if mode = "D" then begin
      let n = Scanf.scanf " %d" (fun x -> x) in
      let d = Array.make (max (n + 1) 2) 0L in
      d.(0) <- 1L;
      if n >= 1 then d.(1) <- 0L;
      for i = 2 to n do
        d.(i) <- Int64.mul (Int64.of_int (i - 1)) (Int64.add d.(i - 1) d.(i - 2))
      done;
      Printf.printf "%Ld\n" d.(n)
    end else begin
      let rows = Scanf.scanf " %d" (fun x -> x) in
      let cols = Scanf.scanf " %d" (fun x -> x) in
      let b = Array.make_matrix rows cols 0 in
      for i = 0 to rows - 1 do
        for j = 0 to cols - 1 do
          b.(i).(j) <- Scanf.scanf " %d" (fun x -> x)
        done
      done;
      let total_masks = 1 lsl cols in
      let dp = ref (Array.make total_masks 0L) in
      !dp.(0) <- 1L;
      for i = 0 to rows - 1 do
        let ndp = Array.copy !dp in
        for mask = 0 to total_masks - 1 do
          let cur = !dp.(mask) in
          if cur <> 0L then begin
            for j = 0 to cols - 1 do
              if b.(i).(j) <> 0 && (mask land (1 lsl j)) = 0 then
                let next_mask = mask lor (1 lsl j) in
                ndp.(next_mask) <- Int64.add ndp.(next_mask) cur
            done
          end
        done;
        dp := ndp
      done;
      let m = min rows cols in
      let r = Array.make (m + 1) 0L in
      for mask = 0 to total_masks - 1 do
        let c = ref 0 in
        for j = 0 to cols - 1 do
          if (mask land (1 lsl j)) <> 0 then incr c
        done;
        r.(!c) <- Int64.add r.(!c) !dp.(mask)
      done;
      for i = 0 to m do
        if i > 0 then print_char ' ';
        Printf.printf "%Ld" r.(i)
      done;
      print_newline ()
    end
  with End_of_file -> ()
