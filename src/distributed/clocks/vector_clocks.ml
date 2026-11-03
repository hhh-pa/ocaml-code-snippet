let compare_clocks a b =
  let a_le_b = ref true in
  let b_le_a = ref true in
  let len = Array.length a in
  for i = 0 to len - 1 do
    if a.(i) > b.(i) then a_le_b := false;
    if b.(i) > a.(i) then b_le_a := false
  done;
  if !a_le_b && !b_le_a then 2
  else if !a_le_b then 1
  else if !b_le_a then -1
  else 0

let () =
  try
    let n_procs = Scanf.scanf " %d" (fun x -> x) in
    let n_ops = Scanf.scanf " %d" (fun x -> x) in
    let vc = Array.init n_procs (fun _ -> Array.make n_procs 0) in
    let event_clocks = Hashtbl.create 64 in
    let in_flight_messages = Hashtbl.create 64 in
    for _ = 0 to n_ops - 1 do
      let op = Scanf.scanf " %s" (fun x -> x) in
      if op = "LOCAL" then begin
        let p = Scanf.scanf " %d" (fun x -> x) in
        let name = Scanf.scanf " %s" (fun x -> x) in
        vc.(p).(p) <- vc.(p).(p) + 1;
        Hashtbl.replace event_clocks name (Array.copy vc.(p))
      end else if op = "SEND" then begin
        let from_p = Scanf.scanf " %d" (fun x -> x) in
        let msg_id = Scanf.scanf " %s" (fun x -> x) in
        vc.(from_p).(from_p) <- vc.(from_p).(from_p) + 1;
        Hashtbl.replace in_flight_messages msg_id (Array.copy vc.(from_p));
        Hashtbl.replace event_clocks (msg_id ^ "_SEND") (Array.copy vc.(from_p))
      end else if op = "RECV" then begin
        let to_p = Scanf.scanf " %d" (fun x -> x) in
        let msg_id = Scanf.scanf " %s" (fun x -> x) in
        let msg_vc = Hashtbl.find in_flight_messages msg_id in
        for k = 0 to n_procs - 1 do
          vc.(to_p).(k) <- max vc.(to_p).(k) msg_vc.(k)
        done;
        vc.(to_p).(to_p) <- vc.(to_p).(to_p) + 1;
        Hashtbl.replace event_clocks (msg_id ^ "_RECV") (Array.copy vc.(to_p))
      end
    done;
    try
      let n_queries = Scanf.scanf " %d" (fun x -> x) in
      for _ = 0 to n_queries - 1 do
        let e1 = Scanf.scanf " %s" (fun x -> x) in
        let e2 = Scanf.scanf " %s" (fun x -> x) in
        let v1 = Hashtbl.find event_clocks e1 in
        let v2 = Hashtbl.find event_clocks e2 in
        let rel = compare_clocks v1 v2 in
        if rel = 1 then
          Printf.printf "%s -> %s\n" e1 e2
        else if rel = -1 then
          Printf.printf "%s -> %s\n" e2 e1
        else if rel = 2 then
          Printf.printf "%s == %s\n" e1 e2
        else
          Printf.printf "%s || %s\n" e1 e2
      done
    with End_of_file -> ()
  with End_of_file -> ()
