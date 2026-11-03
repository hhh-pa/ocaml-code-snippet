let ll_prime p =
  if p = 2 then true
  else begin
    let m = Int64.sub (Int64.shift_left 1L p) 1L in
    let s = ref 4L in
    for _ = 0 to p - 3 do
      let sq = Int64.rem (Int64.mul !s !s) m in
      let sub2 = Int64.sub sq 2L in
      let res = Int64.rem sub2 m in
      s := (if res < 0L then Int64.add res m else res)
    done;
    !s = 0L
  end

let () =
  let rec loop () =
    try
      let p = Scanf.scanf " %d" (fun x -> x) in
      Printf.printf "%s\n" (if ll_prime p then "PRIME" else "COMPOSITE");
      loop ()
    with End_of_file -> ()
  in
  loop ()
