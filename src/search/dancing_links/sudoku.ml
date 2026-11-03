let box_id r c = (r / 3) * 3 + (c / 3)

let solve_board () =
  let map = Array.make_matrix 9 9 0 in
  let rows = Array.make 9 0 in
  let cols = Array.make 9 0 in
  let boxes = Array.make 9 0 in

  for i = 0 to 8 do
    for j = 0 to 8 do
      let v = Scanf.scanf " %d" (fun x -> x) in
      map.(i).(j) <- v;
      if v <> 0 then begin
        let bit = 1 lsl v in
        rows.(i) <- rows.(i) lor bit;
        cols.(j) <- cols.(j) lor bit;
        boxes.(box_id i j) <- boxes.(box_id i j) lor bit
      end
    done
  done;

  let rec solve () =
    let best_row = ref (-1) in
    let best_col = ref (-1) in
    let best_count = ref 10 in
    let best_mask = ref 0 in

    let r = ref 0 in
    while !r < 9 && !best_count > 1 do
      let c = ref 0 in
      while !c < 9 && !best_count > 1 do
        if map.(!r).(!c) = 0 then begin
          let used = rows.(!r) lor cols.(!c) lor boxes.(box_id !r !c) in
          let mask = ref 0 in
          let count = ref 0 in
          for d = 1 to 9 do
            if (used land (1 lsl d)) = 0 then begin
              mask := !mask lor (1 lsl d);
              incr count
            end
          done;
          if !count = 0 then begin
            best_count := 0;
            best_row := !r;
            best_col := !c
          end else if !count < !best_count then begin
            best_count := !count;
            best_row := !r;
            best_col := !c;
            best_mask := !mask
          end
        end;
        incr c
      done;
      incr r
    done;

    if !best_count = 0 then false
    else if !best_row = -1 then true
    else begin
      let br = !best_row in
      let bc = !best_col in
      let bmask = !best_mask in
      let b = box_id br bc in
      let found = ref false in
      let d = ref 1 in
      while !d <= 9 && not !found do
        let bit = 1 lsl !d in
        if (bmask land bit) <> 0 then begin
          map.(br).(bc) <- !d;
          rows.(br) <- rows.(br) lor bit;
          cols.(bc) <- cols.(bc) lor bit;
          boxes.(b) <- boxes.(b) lor bit;
          if solve () then found := true
          else begin
            rows.(br) <- rows.(br) land (lnot bit);
            cols.(bc) <- cols.(bc) land (lnot bit);
            boxes.(b) <- boxes.(b) land (lnot bit);
            map.(br).(bc) <- 0
          end
        end;
        incr d
      done;
      !found
    end
  in
  let _ = solve () in
  for i = 0 to 8 do
    for j = 0 to 8 do
      Printf.printf "%d " map.(i).(j)
    done;
    Printf.printf "\n"
  done

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    for _ = 0 to n - 1 do
      solve_board ()
    done
  with End_of_file -> ()
