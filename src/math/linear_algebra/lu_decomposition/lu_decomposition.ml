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
    let a = Array.make_matrix n n 0L in
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        a.(i).(j) <- mod_norm (Scanf.scanf " %Ld" (fun x -> x))
      done
    done;
    let b = Array.init n (fun _ -> mod_norm (Scanf.scanf " %Ld" (fun x -> x))) in

    let pi = Array.init n (fun i -> i) in
    let swaps = ref 0 in

    for k = 0 to n - 1 do
      let p = ref 0L in
      let kp = ref k in
      for i = k to n - 1 do
        if a.(i).(k) > !p then begin
          p := a.(i).(k);
          kp := i
        end
      done;
      if !p = 0L then begin
        Printf.printf "SINGULAR\n";
        exit 0
      end;
      if !kp <> k then begin
        incr swaps;
        let tmp_pi = pi.(k) in
        pi.(k) <- pi.(!kp);
        pi.(!kp) <- tmp_pi;
        let tmp_row = a.(k) in
        a.(k) <- a.(!kp);
        a.(!kp) <- tmp_row
      end;
      let inv = mod_pow a.(k).(k) (Int64.sub mod_val 2L) in
      for i = k + 1 to n - 1 do
        a.(i).(k) <- Int64.rem (Int64.mul a.(i).(k) inv) mod_val;
        let factor = a.(i).(k) in
        for j = k + 1 to n - 1 do
          let prod = Int64.rem (Int64.mul factor a.(k).(j)) mod_val in
          a.(i).(j) <- mod_norm (Int64.sub a.(i).(j) prod)
        done
      done
    done;

    let det = ref 1L in
    for k = 0 to n - 1 do
      det := Int64.rem (Int64.mul !det a.(k).(k)) mod_val
    done;
    if (!swaps land 1) = 1 && !det <> 0L then
      det := Int64.sub mod_val !det;

    (* solve Ly = Pb *)
    let y = Array.make n 0L in
    for i = 0 to n - 1 do
      let s = ref b.(pi.(i)) in
      for j = 0 to i - 1 do
        let prod = Int64.rem (Int64.mul a.(i).(j) y.(j)) mod_val in
        s := mod_norm (Int64.sub !s prod)
      done;
      y.(i) <- !s
    done;

    (* solve Ux = y *)
    let x = Array.make n 0L in
    for i = n - 1 downto 0 do
      let s = ref y.(i) in
      for j = i + 1 to n - 1 do
        let prod = Int64.rem (Int64.mul a.(i).(j) x.(j)) mod_val in
        s := mod_norm (Int64.sub !s prod)
      done;
      let inv_diag = mod_pow a.(i).(i) (Int64.sub mod_val 2L) in
      x.(i) <- Int64.rem (Int64.mul !s inv_diag) mod_val
    done;

    (* inverse *)
    let inv_mat = Array.make_matrix n n 0L in
    for c = 0 to n - 1 do
      let yy = Array.make n 0L in
      for i = 0 to n - 1 do
        let s = ref (if pi.(i) = c then 1L else 0L) in
        for j = 0 to i - 1 do
          let prod = Int64.rem (Int64.mul a.(i).(j) yy.(j)) mod_val in
          s := mod_norm (Int64.sub !s prod)
        done;
        yy.(i) <- !s
      done;
      let col = Array.make n 0L in
      for i = n - 1 downto 0 do
        let s = ref yy.(i) in
        for j = i + 1 to n - 1 do
          let prod = Int64.rem (Int64.mul a.(i).(j) col.(j)) mod_val in
          s := mod_norm (Int64.sub !s prod)
        done;
        let inv_diag = mod_pow a.(i).(i) (Int64.sub mod_val 2L) in
        col.(i) <- Int64.rem (Int64.mul !s inv_diag) mod_val
      done;
      for r = 0 to n - 1 do
        inv_mat.(r).(c) <- col.(r)
      done
    done;

    Printf.printf "%Ld\n" !det;
    for i = 0 to n - 1 do
      if i > 0 then print_string " ";
      Printf.printf "%Ld" x.(i)
    done;
    print_newline ();
    for r = 0 to n - 1 do
      for c = 0 to n - 1 do
        if c > 0 then print_string " ";
        Printf.printf "%Ld" inv_mat.(r).(c)
      done;
      print_newline ()
    done
  with End_of_file -> ()
