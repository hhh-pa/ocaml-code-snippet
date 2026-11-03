let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let id_arr = Array.init n (fun _ -> Scanf.scanf " %Ld" (fun x -> x)) in
    let mn = ref id_arr.(0) in
    for i = 1 to n - 1 do
      if id_arr.(i) < !mn then mn := id_arr.(i)
    done;
    let n_l = Int64.of_int n in
    let ts_rounds = Int64.add (Int64.mul !mn n_l) n_l in
    let vs_rounds = Int64.mul n_l (Int64.add !mn 1L) in
    let vs_msgs = ref n_l in
    for i = 0 to n - 1 do
      let x = id_arr.(i) in
      if x <> !mn then begin
        let h = Int64.div vs_rounds (Int64.add x 1L) in
        let h = if h > n_l then n_l else h in
        vs_msgs := Int64.add !vs_msgs h
      end
    done;
    Printf.printf "TIME-SLICE ROUNDS: %Ld MESSAGES: %d\n" ts_rounds n;
    Printf.printf "VARIABLE-SPEED ROUNDS: %Ld MESSAGES: %Ld\n" vs_rounds !vs_msgs;
    Printf.printf "LEADER: %Ld\n" !mn
  with End_of_file -> ()
