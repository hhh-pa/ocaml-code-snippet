let () =
  try
    let q = Scanf.scanf " %d" (fun x -> x) in
    let a = Array.make (max 1 q) 0L in
    let sz = ref 0 in
    let cap = ref 1L in
    for _ = 1 to q do
      let op = Scanf.scanf " %d" (fun x -> x) in
      if op = 1 then begin
        let x = Scanf.scanf " %Ld" (fun x -> x) in
        if Int64.of_int !sz = !cap then
          cap := Int64.mul !cap 2L;
        a.(!sz) <- x;
        incr sz
      end else if op = 2 then begin
        if !sz = 0 then
          Printf.printf "EMPTY\n"
        else begin
          decr sz;
          Printf.printf "%Ld\n" a.(!sz);
          if !sz > 0 && Int64.mul (Int64.of_int !sz) 4L <= !cap && !cap > 1L then
            cap := Int64.div !cap 2L
        end
      end else if op = 3 then begin
        let j = Scanf.scanf " %Ld" (fun x -> x) in
        if j < 0L || j >= Int64.of_int !sz then
          Printf.printf "NONE\n"
        else
          Printf.printf "%Ld\n" a.(Int64.to_int j)
      end else if op = 4 then begin
        Printf.printf "%d %Ld\n" !sz !cap
      end
    done
  with End_of_file -> ()
