let morris_inorder val_ left right_orig =
  let right = Array.copy right_orig in
  let curr = ref 0 in
  let first = ref true in
  while !curr <> -1 do
    let u = !curr in
    if left.(u) = -1 then begin
      if not !first then print_string " ";
      first := false;
      Printf.printf "%Ld" val_.(u);
      curr := right.(u)
    end else begin
      let pred = ref left.(u) in
      while right.(!pred) <> -1 && right.(!pred) <> u do
        pred := right.(!pred)
      done;
      let p = !pred in
      if right.(p) = -1 then begin
        right.(p) <- u;
        curr := left.(u)
      end else begin
        right.(p) <- -1;
        if not !first then print_string " ";
        first := false;
        Printf.printf "%Ld" val_.(u);
        curr := right.(u)
      end
    end
  done;
  print_newline ()

let morris_preorder val_ left right_orig =
  let right = Array.copy right_orig in
  let curr = ref 0 in
  let first = ref true in
  while !curr <> -1 do
    let u = !curr in
    if left.(u) = -1 then begin
      if not !first then print_string " ";
      first := false;
      Printf.printf "%Ld" val_.(u);
      curr := right.(u)
    end else begin
      let pred = ref left.(u) in
      while right.(!pred) <> -1 && right.(!pred) <> u do
        pred := right.(!pred)
      done;
      let p = !pred in
      if right.(p) = -1 then begin
        if not !first then print_string " ";
        first := false;
        Printf.printf "%Ld" val_.(u);
        right.(p) <- u;
        curr := left.(u)
      end else begin
        right.(p) <- -1;
        curr := right.(u)
      end
    end
  done;
  print_newline ()

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    if n > 0 then begin
      let val_ = Array.make n 0L in
      let left = Array.make n (-1) in
      let right = Array.make n (-1) in
      for i = 0 to n - 1 do
        val_.(i) <- Scanf.scanf " %Ld" (fun x -> x);
        left.(i) <- Scanf.scanf " %d" (fun x -> x);
        right.(i) <- Scanf.scanf " %d" (fun x -> x)
      done;
      morris_inorder val_ left right;
      morris_preorder val_ left right
    end
  with End_of_file -> ()
