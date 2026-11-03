let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let q = Scanf.scanf " %d" (fun x -> x) in
    let state = Array.make n 0L in
    let sn = ref 0 in
    for _ = 0 to q - 1 do
      let op = Scanf.scanf " %s" (fun x -> x) in
      if op = "U" then begin
        let i = Scanf.scanf " %d" (fun x -> x) in
        let v = Scanf.scanf " %Ld" (fun x -> x) in
        state.(i) <- v
      end else begin
        let c = Scanf.scanf " %d" (fun x -> x) in
        let c1 = Array.copy state in
        for _ = 0 to c - 1 do
          let u = Scanf.scanf " %d" (fun x -> x) in
          let v = Scanf.scanf " %Ld" (fun x -> x) in
          state.(u) <- v
        done;
        let c2 = Array.copy state in
        let collects = if c1 = c2 then 2 else 3 in
        Printf.printf "SCAN %d:" !sn;
        for i = 0 to n - 1 do
          Printf.printf " %Ld" c2.(i)
        done;
        Printf.printf " collects=%d\n" collects;
        incr sn
      end
    done;
    Printf.printf "STATE:";
    for i = 0 to n - 1 do
      Printf.printf " %Ld" state.(i)
    done;
    Printf.printf "\n"
  with End_of_file -> ()
