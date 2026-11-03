let attacking_pairs b n =
  let cnt = ref 0 in
  for i = 0 to n - 1 do
    for j = i + 1 to n - 1 do
      if b.(i) = b.(j) then incr cnt
      else if abs (b.(i) - b.(j)) = j - i then incr cnt
    done
  done;
  !cnt

let solve_case () =
  let n = Scanf.scanf " %d" (fun x -> x) in
  let board = Array.make n 0 in
  for i = 0 to n - 1 do
    board.(i) <- Scanf.scanf " %d" (fun x -> x)
  done;

  let stop = ref false in
  while not !stop do
    let cur = attacking_pairs board n in
    if cur = 0 then stop := true
    else begin
      let best_val = ref cur in
      let best_row = ref (-1) in
      let best_col = ref (-1) in
      for r = 0 to n - 1 do
        for c = 0 to n - 1 do
          if c <> board.(r) then begin
            let old_val = board.(r) in
            board.(r) <- c;
            let v = attacking_pairs board n in
            board.(r) <- old_val;
            if v < !best_val then begin
              best_val := v;
              best_row := r;
              best_col := c
            end
          end
        done
      done;
      if !best_row = -1 then stop := true
      else board.(!best_row) <- !best_col
    end
  done;

  for i = 0 to n - 1 do
    if i > 0 then Printf.printf " ";
    Printf.printf "%d" board.(i)
  done;
  Printf.printf "\n%d\n" (attacking_pairs board n)

let () =
  try
    let t = Scanf.scanf " %d" (fun x -> x) in
    for _ = 0 to t - 1 do
      solve_case ()
    done
  with End_of_file -> ()
