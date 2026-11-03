let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let m = Scanf.scanf " %d" (fun x -> x) in
    let adj = Array.make n [] in
    for _ = 1 to m do
      let u = Scanf.scanf " %d" (fun x -> x) in
      let v = Scanf.scanf " %d" (fun x -> x) in
      adj.(u) <- v :: adj.(u)
    done;
    for u = 0 to n - 1 do
      adj.(u) <- List.sort compare adj.(u)
    done;
    let mt_r = Array.make n (-1) in
    let seen = Array.make n false in
    let rec bpm u =
      let rec try_adj = function
        | [] -> false
        | v :: rest ->
          if seen.(v) then
            try_adj rest
          else begin
            seen.(v) <- true;
            if mt_r.(v) = -1 || bpm mt_r.(v) then begin
              mt_r.(v) <- u;
              true
            end else
              try_adj rest
          end
      in
      try_adj adj.(u)
    in
    for u = 0 to n - 1 do
      Array.fill seen 0 n false;
      ignore (bpm u)
    done;
    let mt_l = Array.make n (-1) in
    for v = 0 to n - 1 do
      if mt_r.(v) <> -1 then
        mt_l.(mt_r.(v)) <- v
    done;
    let paths = ref [] in
    for s = 0 to n - 1 do
      if mt_r.(s) = -1 then begin
        let p = ref [s] in
        let cur = ref s in
        while mt_l.(!cur) <> -1 do
          cur := mt_l.(!cur);
          p := !cur :: !p
        done;
        paths := List.rev !p :: !paths
      end
    done;
    let sorted_paths = List.sort (fun p1 p2 -> compare (List.hd p1) (List.hd p2)) !paths in
    Printf.printf "%d\n" (List.length sorted_paths);
    List.iter (fun p ->
      let first = ref true in
      List.iter (fun x ->
        if not !first then print_string " ";
        print_int x;
        first := false
      ) p;
      print_newline ()
    ) sorted_paths
  with End_of_file -> ()
