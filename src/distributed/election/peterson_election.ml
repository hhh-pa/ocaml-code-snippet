let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let id_arr = Array.init n (fun _ -> Scanf.scanf " %Ld" (fun x -> x)) in
    let cur = Array.copy id_arr in
    let active = Array.make n 1 in
    let leader = ref (-1L) in
    let alias = ref (-1L) in
    let phases = ref 0 in
    let cap = 4 * n + 10 in
    let loop_done = ref false in
    while !phases < cap && not !loop_done do
      let a = ref [] in
      for i = 0 to n - 1 do
        if active.(i) = 1 then a := i :: !a
      done;
      let a_arr = Array.of_list (List.rev !a) in
      let k = Array.length a_arr in
      if k = 0 then
        loop_done := true
      else begin
        incr phases;
        if k = 1 then begin
          leader := id_arr.(a_arr.(0));
          alias := cur.(a_arr.(0));
          loop_done := true
        end else begin
          let ncur = Array.copy cur in
          let nact = Array.copy active in
          let done_flag = ref false in
          for j = 0 to k - 1 do
            if not !done_flag then begin
              let q = a_arr.(j) in
              let uid2 = cur.(a_arr.((j - 1 + k) mod k)) in
              let uid3 = cur.(a_arr.((j - 2 + k) mod k)) in
              if uid2 = cur.(q) then begin
                leader := id_arr.(q);
                alias := cur.(q);
                done_flag := true
              end else if uid2 > cur.(q) && uid2 > uid3 then
                ncur.(q) <- uid2
              else
                nact.(q) <- 0
            end
          done;
          Array.blit ncur 0 cur 0 n;
          Array.blit nact 0 active 0 n;
          if !done_flag then loop_done := true
        end
      end
    done;
    let msgs = Int64.mul (Int64.mul 2L (Int64.of_int n)) (Int64.of_int !phases) in
    Printf.printf "LEADER: %Ld\n" !leader;
    Printf.printf "ALIAS: %Ld\n" !alias;
    Printf.printf "PHASES: %d\n" !phases;
    Printf.printf "MESSAGES: %Ld\n" msgs
  with End_of_file -> ()
