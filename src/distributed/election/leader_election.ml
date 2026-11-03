let simulate_lcr ring =
  let n = Array.length ring in
  let messages = ref 0 in
  for i = 0 to n - 1 do
    let uid = ring.(i) in
    let cur = ref i in
    let continue_loop = ref true in
    while !continue_loop do
      incr messages;
      let nxt = (!cur + 1) mod n in
      if nxt = i then
        continue_loop := false
      else if ring.(nxt) > uid then
        continue_loop := false
      else
        cur := nxt
    done
  done;
  !messages

let simulate_hs ring =
  let n = Array.length ring in
  let messages = ref 0 in
  let phase = ref 0 in
  let stop = ref false in
  while not !stop do
    let limit = 1 lsl !phase in
    let leader_found = ref false in
    let i = ref 0 in
    while !i < n && not !leader_found do
      let uid = ring.(!i) in
      let dirs = [-1; 1] in
      let rec check_dirs = function
        | [] -> ()
        | dir :: rest ->
          let dist = ref 0 in
          let cur = ref !i in
          let ok = ref true in
          let inner_stop = ref false in
          while !dist < limit && not !inner_stop do
            incr dist;
            incr messages;
            cur := (!cur + dir + n) mod n;
            if !cur = !i then begin
              leader_found := true;
              inner_stop := true
            end else if ring.(!cur) > uid then begin
              ok := false;
              inner_stop := true
            end
          done;
          if not !leader_found then begin
            if !ok then
              messages := !messages + !dist;
            check_dirs rest
          end
      in
      check_dirs dirs;
      incr i
    done;
    if !leader_found || limit >= n / 2 then
      stop := true
    else
      incr phase
  done;
  !messages

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let ring = Array.init n (fun _ -> Scanf.scanf " %d" (fun x -> x)) in
    let leader_id = ref ring.(0) in
    for i = 1 to n - 1 do
      if ring.(i) > !leader_id then leader_id := ring.(i)
    done;
    let lcr_msgs = simulate_lcr ring in
    let hs_msgs = simulate_hs ring in
    Printf.printf "LEADER: %d\n" !leader_id;
    Printf.printf "LCR MESSAGES: %d\n" lcr_msgs;
    Printf.printf "HS MESSAGES: %d\n" hs_msgs
  with End_of_file -> ()
