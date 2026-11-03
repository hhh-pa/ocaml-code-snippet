let r_val = 256

let () =
  try
    let a = Scanf.scanf " %d" (fun x -> x) in
    let b = Scanf.scanf " %d" (fun x -> x) in
    let m = Scanf.scanf " %d" (fun x -> x) in

    let minv = ref 1 in
    let x = ref 1 in
    while !x < r_val do
      if (!x * m) mod r_val = 1 then begin
        minv := !x;
        x := r_val
      end else
        incr x
    done;

    let mp = (r_val - !minv) mod r_val in

    let redc t =
      let q = ((t land 255) * mp) land 255 in
      let t_out = (t + q * m) lsr 8 in
      if t_out >= m then t_out - m else t_out
    in

    let a_mod_m = ((a mod m) + m) mod m in
    let b_mod_m = ((b mod m) + m) mod m in
    let a_m = (a_mod_m * r_val) mod m in
    let b_m = (b_mod_m * r_val) mod m in
    let ab_m = redc (a_m * b_m) in
    let ab = redc ab_m in

    Printf.printf "%d %d\n" a_m b_m;
    Printf.printf "%d\n" ab_m;
    Printf.printf "%d\n" ab
  with End_of_file -> ()
