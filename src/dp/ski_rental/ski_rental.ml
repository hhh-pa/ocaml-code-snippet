let () =
  try
    let b = Scanf.scanf " %Ld" (fun x -> x) in
    let t = Scanf.scanf " %Ld" (fun x -> x) in
    let off = min t b in
    let det =
      if t < b then t
      else Int64.add (Int64.sub b 1L) b
    in
    let rnd =
      if t >= b then
        let b2 = Int64.mul b b in
        let b_b1_div2 = Int64.div (Int64.mul b (Int64.sub b 1L)) 2L in
        Int64.add b2 b_b1_div2
      else
        let t_t1_div2 = Int64.div (Int64.mul t (Int64.sub t 1L)) 2L in
        let term2 = Int64.mul t (Int64.sub (Int64.mul 2L b) t) in
        Int64.add t_t1_div2 term2
    in
    Printf.printf "%Ld\n%Ld\n%Ld\n" off det rnd
  with End_of_file -> ()
