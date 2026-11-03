let rec egcd a b =
  if b = 0L then (a, 1L, 0L)
  else
    let q = Int64.div a b in
    let r = Int64.rem a b in
    let (g, x1, y1) = egcd b r in
    (g, y1, Int64.sub x1 (Int64.mul q y1))

let () =
  try
    let a = Scanf.scanf " %Ld" (fun x -> x) in
    let b = Scanf.scanf " %Ld" (fun x -> x) in
    let n = Scanf.scanf " %Ld" (fun x -> x) in

    if n <= 0L then
      Printf.printf "NONE\n"
    else if n = 1L then
      Printf.printf "0\n"
    else begin
      let norm v =
        let r = Int64.rem v n in
        if r < 0L then Int64.add r n else r
      in
      let a = norm a in
      let b = norm b in
      let (g, x, _) = egcd a n in
      if Int64.rem b g <> 0L then
        Printf.printf "NONE\n"
      else begin
        let n2 = Int64.div n g in
        let b_div_g = Int64.div b g in
        let x_mul = Int64.rem (Int64.mul (Int64.rem x n) b_div_g) n in
        let x0 = if x_mul < 0L then Int64.add x_mul n else x_mul in
        let sols = ref [] in
        let k = ref 0L in
        while !k < g do
          let sol = Int64.rem (Int64.add x0 (Int64.mul !k n2)) n in
          sols := sol :: !sols;
          k := Int64.add !k 1L
        done;
        let sorted_sols = List.sort compare !sols in
        Printf.printf "%Ld\n" g;
        let first = ref true in
        List.iter (fun s ->
          if not !first then print_string " ";
          Printf.printf "%Ld" s;
          first := false
        ) sorted_sols;
        print_newline ()
      end
    end
  with End_of_file -> ()
