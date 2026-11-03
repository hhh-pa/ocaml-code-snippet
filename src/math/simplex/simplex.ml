let eps = 1e-9
let inf = 1e18

let () =
  try
    let m = Scanf.scanf " %d" (fun x -> x) in
    let n = Scanf.scanf " %d" (fun x -> x) in

    let tab = Array.make_matrix (m + 1) (n + m + 1) 0.0 in

    for j = 0 to n - 1 do
      let c = Scanf.scanf " %f" (fun x -> x) in
      tab.(m).(j) <- -. c
    done;

    for i = 0 to m - 1 do
      for j = 0 to n - 1 do
        tab.(i).(j) <- Scanf.scanf " %f" (fun x -> x)
      done;
      tab.(i).(n + i) <- 1.0;
      tab.(i).(n + m) <- Scanf.scanf " %f" (fun x -> x)
    done;

    let rec loop () =
      let enter_col = ref (-1) in
      let min_val = ref (-. eps) in
      for j = 0 to n + m - 1 do
        if tab.(m).(j) < !min_val then begin
          min_val := tab.(m).(j);
          enter_col := j
        end
      done;

      if !enter_col = -1 then begin
        Printf.printf "OPTIMAL\n";
        Printf.printf "%.2f\n" tab.(m).(n + m)
      end else begin
        let leave_row = ref (-1) in
        let min_ratio = ref inf in
        for i = 0 to m - 1 do
          let coeff = tab.(i).(!enter_col) in
          if coeff > eps then begin
            let ratio = tab.(i).(n + m) /. coeff in
            if ratio < !min_ratio -. eps then begin
              min_ratio := ratio;
              leave_row := i
            end
          end
        done;

        if !leave_row = -1 then begin
          Printf.printf "UNBOUNDED\n"
        end else begin
          let pivot_val = tab.(!leave_row).(!enter_col) in
          for j = 0 to n + m do
            tab.(!leave_row).(j) <- tab.(!leave_row).(j) /. pivot_val
          done;

          for i = 0 to m do
            if i <> !leave_row then begin
              let factor = tab.(i).(!enter_col) in
              if abs_float factor > eps then begin
                for j = 0 to n + m do
                  tab.(i).(j) <- tab.(i).(j) -. factor *. tab.(!leave_row).(j)
                done
              end
            end
          done;
          loop ()
        end
      end
    in
    loop ()
  with End_of_file -> ()
