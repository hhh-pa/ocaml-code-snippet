let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let f = Scanf.scanf " %d" (fun x -> x) in
    let rmax = Scanf.scanf " %d" (fun x -> x) in
    let x = Array.init n (fun _ -> Scanf.scanf " %d" (fun v -> v)) in
    let k = Scanf.scanf " %d" (fun v -> v) in
    let faulty = Array.make n false in
    for _ = 0 to k - 1 do
      let t = Scanf.scanf " %d" (fun v -> v) in
      faulty.(t) <- true
    done;
    let lcg = ref 777L in
    let flip () =
      lcg := Int64.add (Int64.mul !lcg 6364136223846793005L) 1442695040888963407L;
      Int64.to_int (Int64.logand (Int64.shift_right_logical !lcg 33) 1L)
    in
    let nc = ref 0 in
    for i = 0 to n - 1 do
      if not faulty.(i) then incr nc
    done;
    let decision = ref (-1) in
    let rounds = ref 0 in
    let flips = ref 0L in
    let r = ref 1 in
    while !r <= rmax && !decision < 0 do
      rounds := !r;
      let o = ref 0 in
      let z = ref 0 in
      for i = 0 to n - 1 do
        if not faulty.(i) then begin
          if x.(i) = 1 then incr o else incr z
        end
      done;
      let prop = ref (-1) in
      if !o > n / 2 then prop := 1
      else if !z > n / 2 then prop := 0;
      if !prop >= 0 then begin
        if !nc >= f + 1 then
          decision := !prop
        else begin
          for i = 0 to n - 1 do
            if not faulty.(i) then x.(i) <- !prop
          done
        end
      end else begin
        for i = 0 to n - 1 do
          if not faulty.(i) then begin
            x.(i) <- flip ();
            flips := Int64.add !flips 1L
          end
        done
      end;
      incr r
    done;
    Printf.printf "DECISION: ";
    if !decision >= 0 then
      Printf.printf "%d\n" !decision
    else
      Printf.printf "UNDECIDED\n";
    Printf.printf "ROUNDS: %d\n" !rounds;
    Printf.printf "COINFLIPS: %Ld\n" !flips
  with End_of_file -> ()
