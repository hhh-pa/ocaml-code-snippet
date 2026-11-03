let dirs = [
  (-1, -1); (-1, 0); (-1, 1);
  (0, -1);           (0, 1);
  (1, -1);  (1, 0);  (1, 1)
]

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let m = Scanf.scanf " %d" (fun x -> x) in
    let grid = Array.make_matrix n m 0 in
    for i = 0 to n - 1 do
      for j = 0 to m - 1 do
        grid.(i).(j) <- Scanf.scanf " %d" (fun x -> x)
      done
    done;

    let measure_region r c =
      let stack = ref [(r, c)] in
      grid.(r).(c) <- -1;
      let size = ref 0 in
      while !stack <> [] do
        match !stack with
        | [] -> ()
        | (cr, cc) :: rest ->
            stack := rest;
            incr size;
            List.iter (fun (dr, dc) ->
              let nr = cr + dr in
              let nc = cc + dc in
              if nr >= 0 && nr < n && nc >= 0 && nc < m && grid.(nr).(nc) = 1 then begin
                grid.(nr).(nc) <- -1;
                stack := (nr, nc) :: !stack
              end
            ) dirs
      done;
      !size
    in

    let max_region = ref (-1) in
    for r = 0 to n - 1 do
      for c = 0 to m - 1 do
        if grid.(r).(c) = 1 then begin
          let s = measure_region r c in
          if s > !max_region then max_region := s
        end
      done
    done;
    Printf.printf "%d\n" !max_region
  with End_of_file -> ()
