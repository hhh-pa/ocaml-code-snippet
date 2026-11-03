let () =
  try
    let m = Scanf.scanf " %d" (fun x -> x) in
    let n = Scanf.scanf " %d" (fun x -> x) in
    if m <= 0 || n <= 0 then exit 0;
    let a = Array.make_matrix m n 0.0 in
    for i = 0 to m - 1 do
      for j = 0 to n - 1 do
        a.(i).(j) <- Scanf.scanf " %f" (fun x -> x)
      done
    done;
    let b = Array.init m (fun _ -> Scanf.scanf " %f" (fun x -> x)) in

    let ata = Array.make_matrix n n 0.0 in
    let atb = Array.make n 0.0 in
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        let s = ref 0.0 in
        for k = 0 to m - 1 do
          s := !s +. a.(k).(i) *. a.(k).(j)
        done;
        ata.(i).(j) <- !s
      done;
      let s = ref 0.0 in
      for k = 0 to m - 1 do
        s := !s +. a.(k).(i) *. b.(k)
      done;
      atb.(i) <- !s
    done;

    let aug = Array.make_matrix n (n + 1) 0.0 in
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        aug.(i).(j) <- ata.(i).(j)
      done;
      aug.(i).(n) <- atb.(i)
    done;

    for col = 0 to n - 1 do
      let pivot = ref col in
      let best = ref (abs_float aug.(col).(col)) in
      for row = col + 1 to n - 1 do
        let v = abs_float aug.(row).(col) in
        if v > !best then begin
          best := v;
          pivot := row
        end
      done;
      if !best < 1e-12 then begin
        Printf.printf "SINGULAR\n";
        exit 0
      end;
      if !pivot <> col then begin
        let tmp = aug.(!pivot) in
        aug.(!pivot) <- aug.(col);
        aug.(col) <- tmp
      end;
      let diag = aug.(col).(col) in
      for row = col + 1 to n - 1 do
        let f = aug.(row).(col) /. diag in
        for j = col to n do
          aug.(row).(j) <- aug.(row).(j) -. f *. aug.(col).(j)
        done
      done
    done;

    let x = Array.make n 0.0 in
    for i = n - 1 downto 0 do
      let s = ref aug.(i).(n) in
      for j = i + 1 to n - 1 do
        s := !s -. aug.(i).(j) *. x.(j)
      done;
      x.(i) <- !s /. aug.(i).(i)
    done;

    for i = 0 to n - 1 do
      if i > 0 then print_string " ";
      Printf.printf "%.6f" x.(i)
    done;
    print_newline ()
  with End_of_file -> ()
