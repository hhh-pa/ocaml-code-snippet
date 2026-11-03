let mod_val = 1_000_000_007L

let mod_norm x =
  let r = Int64.rem x mod_val in
  if r < 0L then Int64.add r mod_val else r

let mod_pow base exp =
  let rec loop b e acc =
    if e = 0L then acc
    else
      let new_acc = if Int64.rem e 2L = 1L then Int64.rem (Int64.mul acc b) mod_val else acc in
      let new_b = Int64.rem (Int64.mul b b) mod_val in
      loop new_b (Int64.div e 2L) new_acc
  in
  loop (mod_norm base) exp 1L

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    if n <= 0 then exit 0;
    let aug = Array.make_matrix n (2 * n) 0L in
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        aug.(i).(j) <- mod_norm (Scanf.scanf " %Ld" (fun x -> x))
      done;
      aug.(i).(n + i) <- 1L
    done;

    for col = 0 to n - 1 do
      let pivot = ref (-1) in
      let r = ref col in
      while !pivot = -1 && !r < n do
        if aug.(!r).(col) <> 0L then pivot := !r;
        incr r
      done;
      if !pivot = -1 then begin
        Printf.printf "SINGULAR\n";
        exit 0
      end;
      if !pivot <> col then begin
        let tmp = aug.(!pivot) in
        aug.(!pivot) <- aug.(col);
        aug.(col) <- tmp
      end;
      let inv = mod_pow aug.(col).(col) (Int64.sub mod_val 2L) in
      for j = 0 to 2 * n - 1 do
        aug.(col).(j) <- Int64.rem (Int64.mul aug.(col).(j) inv) mod_val
      done;
      for row = 0 to n - 1 do
        if row <> col && aug.(row).(col) <> 0L then begin
          let f = aug.(row).(col) in
          for j = 0 to 2 * n - 1 do
            let prod = Int64.rem (Int64.mul f aug.(col).(j)) mod_val in
            aug.(row).(j) <- mod_norm (Int64.sub aug.(row).(j) prod)
          done
        end
      done
    done;

    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        if j > 0 then print_string " ";
        Printf.printf "%Ld" aug.(i).(n + j)
      done;
      print_newline ()
    done
  with End_of_file -> ()
