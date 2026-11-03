let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    if n <= 0 then exit 0;
    if n = 1 then begin
      let diag = Scanf.scanf " %f" (fun x -> x) in
      let rhs = Scanf.scanf " %f" (fun x -> x) in
      if abs_float diag < 1e-12 then
        Printf.printf "SINGULAR\n"
      else
        Printf.printf "%.6f\n" (rhs /. diag)
    end else begin
      let lo = Array.init (n - 1) (fun _ -> Scanf.scanf " %f" (fun x -> x)) in
      let diag = Array.init n (fun _ -> Scanf.scanf " %f" (fun x -> x)) in
      let hi = Array.init (n - 1) (fun _ -> Scanf.scanf " %f" (fun x -> x)) in
      let rhs = Array.init n (fun _ -> Scanf.scanf " %f" (fun x -> x)) in

      let cp = Array.make (n - 1) 0.0 in
      let dp = Array.make n 0.0 in

      if abs_float diag.(0) < 1e-12 then begin
        Printf.printf "SINGULAR\n";
        exit 0
      end;
      cp.(0) <- hi.(0) /. diag.(0);
      dp.(0) <- rhs.(0) /. diag.(0);

      for i = 1 to n - 2 do
        let denom = diag.(i) -. lo.(i - 1) *. cp.(i - 1) in
        if abs_float denom < 1e-12 then begin
          Printf.printf "SINGULAR\n";
          exit 0
        end;
        cp.(i) <- hi.(i) /. denom;
        dp.(i) <- (rhs.(i) -. lo.(i - 1) *. dp.(i - 1)) /. denom
      done;

      let denom = diag.(n - 1) -. lo.(n - 2) *. cp.(n - 2) in
      if abs_float denom < 1e-12 then begin
        Printf.printf "SINGULAR\n";
        exit 0
      end;
      dp.(n - 1) <- (rhs.(n - 1) -. lo.(n - 2) *. dp.(n - 2)) /. denom;

      let x = Array.make n 0.0 in
      x.(n - 1) <- dp.(n - 1);
      for i = n - 2 downto 0 do
        x.(i) <- dp.(i) -. cp.(i) *. x.(i + 1)
      done;

      for i = 0 to n - 1 do
        if i > 0 then print_string " ";
        Printf.printf "%.6f" x.(i)
      done;
      print_newline ()
    end
  with End_of_file -> ()
