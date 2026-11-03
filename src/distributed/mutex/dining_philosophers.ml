let lcg_state = ref 12345L

let lcg_next () =
  lcg_state := Int64.add (Int64.mul !lcg_state 6364136223846793005L) 1442695040888963407L;
  !lcg_state

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let rounds = Scanf.scanf " %Ld" (fun x -> x) in
    let fork = Array.make n 0 in
    let hungry = Array.make n 0 in
    let held = Array.make n (-1) in
    let eatst = Array.make n 0 in
    let eats = Array.make n 0L in
    let waits = Array.make n 0L in
    let r = ref 0L in
    while !r < rounds do
      for i = 0 to n - 1 do
        if hungry.(i) = 0 && eatst.(i) = 0 then begin
          let val_s = Int64.shift_right_logical (lcg_next ()) 16 in
          let mod100 = Int64.to_int (Int64.rem val_s 100L) in
          if mod100 < 60 then begin
            hungry.(i) <- 1;
            held.(i) <- -1
          end
        end
      done;
      for i = 0 to n - 1 do
        if hungry.(i) = 1 && held.(i) = -1 then begin
          let bit = Int64.to_int (Int64.logand (Int64.shift_right_logical (lcg_next ()) 33) 1L) in
          let f1 = if bit = 0 then i else (i + 1) mod n in
          if fork.(f1) = 0 then begin
            fork.(f1) <- i + 1;
            held.(i) <- f1
          end
        end
      done;
      for i = 0 to n - 1 do
        if hungry.(i) = 1 && held.(i) <> -1 && eatst.(i) = 0 then begin
          let lf = i in
          let rt = (i + 1) mod n in
          let f2 = if held.(i) = lf then rt else lf in
          if fork.(f2) = 0 then begin
            fork.(f2) <- i + 1;
            eatst.(i) <- 1
          end else begin
            fork.(held.(i)) <- 0;
            held.(i) <- -1
          end
        end
      done;
      for i = 0 to n - 1 do
        if eatst.(i) = 2 then begin
          fork.(i) <- 0;
          fork.((i + 1) mod n) <- 0;
          eatst.(i) <- 0;
          hungry.(i) <- 0;
          eats.(i) <- Int64.add eats.(i) 1L
        end else if eatst.(i) = 1 then
          eatst.(i) <- 2
        else if hungry.(i) = 1 then
          waits.(i) <- Int64.add waits.(i) 1L
      done;
      r := Int64.add !r 1L
    done;
    let tot = ref 0L in
    for i = 0 to n - 1 do
      tot := Int64.add !tot eats.(i)
    done;
    let stuck = ref 0 in
    for i = 0 to n - 1 do
      if fork.(i) <> 0 then incr stuck
    done;
    for i = 0 to n - 1 do
      Printf.printf "P%d: eats=%Ld waits=%Ld\n" i eats.(i) waits.(i)
    done;
    Printf.printf "TOTALEATS: %Ld\n" !tot;
    Printf.printf "STUCK: %d\n" !stuck
  with End_of_file -> ()
