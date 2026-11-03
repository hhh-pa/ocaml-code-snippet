let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let d = Scanf.scanf " %d" (fun x -> x) in
    let k = Scanf.scanf " %d" (fun x -> x) in
    let iters = Scanf.scanf " %d" (fun x -> x) in

    let pts = Array.make_matrix n d 0.0 in
    for i = 0 to n - 1 do
      for j = 0 to d - 1 do
        pts.(i).(j) <- Scanf.scanf " %f" (fun x -> x)
      done
    done;

    let c = Array.make_matrix k d 0.0 in
    for cc = 0 to k - 1 do
      for j = 0 to d - 1 do
        c.(cc).(j) <- pts.(cc).(j)
      done
    done;

    let asg = Array.make n 0 in

    for _ = 1 to iters do
      for i = 0 to n - 1 do
        let best = ref (-1.0) in
        let bi = ref 0 in
        for cc = 0 to k - 1 do
          let dd = ref 0.0 in
          for j = 0 to d - 1 do
            let diff = pts.(i).(j) -. c.(cc).(j) in
            dd := !dd +. diff *. diff
          done;
          if !best < 0.0 || !dd < !best then begin
            best := !dd;
            bi := cc
          end
        done;
        asg.(i) <- !bi
      done;

      for cc = 0 to k - 1 do
        let s = Array.make d 0.0 in
        let cnt = ref 0 in
        for i = 0 to n - 1 do
          if asg.(i) = cc then begin
            incr cnt;
            for j = 0 to d - 1 do
              s.(j) <- s.(j) +. pts.(i).(j)
            done
          end
        done;
        if !cnt > 0 then begin
          let fcnt = float_of_int !cnt in
          for j = 0 to d - 1 do
            c.(cc).(j) <- s.(j) /. fcnt
          done
        end
      done
    done;

    for i = 0 to n - 1 do
      if i > 0 then print_string " ";
      print_int asg.(i)
    done;
    print_newline ();

    for cc = 0 to k - 1 do
      for j = 0 to d - 1 do
        if j > 0 then print_string " ";
        let v = c.(cc).(j) +. 0.0 in
        let clean_v = if abs_float v < 1e-12 then 0.0 else v in
        Printf.printf "%.4f" clean_v
      done;
      print_newline ()
    done
  with End_of_file -> ()
