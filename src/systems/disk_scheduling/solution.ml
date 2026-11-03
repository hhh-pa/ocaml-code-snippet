let read_all_tokens () =
  let content =
    let b = Buffer.create 4096 in
    let buf = Bytes.create 4096 in
    let rec loop () =
      let n = input stdin buf 0 4096 in
      if n > 0 then (
        Buffer.add_subbytes b buf 0 n;
        loop ()
      )
    in
    loop ();
    Buffer.contents b
  in
  let len = String.length content in
  let tokens = ref [] in
  let i = ref 0 in
  while !i < len do
    while !i < len && (let c = content.[!i] in c = ' ' || c = '\t' || c = '\r' || c = '\n') do
      incr i
    done;
    if !i < len then (
      let start = !i in
      while !i < len && (let c = content.[!i] in c <> ' ' && c <> '\t' && c <> '\r' && c <> '\n') do
        incr i
      done;
      tokens := String.sub content start (!i - start) :: !tokens
    )
  done;
  Array.of_list (List.rev !tokens)

let () =
  let tokens = read_all_tokens () in
  if Array.length tokens >= 3 then (
    let head = int_of_string tokens.(0) in
    let maxc = int_of_string tokens.(1) in
    let n = int_of_string tokens.(2) in
    let req = Array.init n (fun i -> int_of_string tokens.(3 + i)) in

    (* FCFS *)
    let tot_fcfs = ref 0 in
    let h = ref head in
    for i = 0 to n - 1 do
      tot_fcfs := !tot_fcfs + abs (req.(i) - !h);
      h := req.(i)
    done;
    let fcfs_strs = Array.to_list (Array.map string_of_int req) in
    print_endline (String.concat " " ("FCFS" :: string_of_int !tot_fcfs :: fcfs_strs));

    (* SSTF *)
    let done_arr = Array.make n false in
    let tot_sstf = ref 0 in
    h := head;
    let ord_sstf = ref [] in
    for _ = 0 to n - 1 do
      let pick = ref (-1) in
      for i = 0 to n - 1 do
        if not done_arr.(i) then (
          if !pick = -1 ||
             abs (req.(i) - !h) < abs (req.(!pick) - !h) ||
             (abs (req.(i) - !h) = abs (req.(!pick) - !h) &&
              (req.(i) < req.(!pick) || (req.(i) = req.(!pick) && i < !pick))) then
            pick := i
        )
      done;
      let p = !pick in
      tot_sstf := !tot_sstf + abs (req.(p) - !h);
      h := req.(p);
      done_arr.(p) <- true;
      ord_sstf := req.(p) :: !ord_sstf
    done;
    let sstf_strs = List.map string_of_int (List.rev !ord_sstf) in
    print_endline (String.concat " " ("SSTF" :: string_of_int !tot_sstf :: sstf_strs));

    let up = ref [] in
    let down = ref [] in
    for i = 0 to n - 1 do
      if req.(i) >= head then up := req.(i) :: !up
      else down := req.(i) :: !down
    done;
    let up = List.sort compare !up in
    let down = List.sort compare !down in

    (* SCAN *)
    let tot_scan = ref (maxc - head) in
    if down <> [] then tot_scan := !tot_scan + (maxc - List.hd down);
    let scan_ord = up @ (List.rev down) in
    let scan_strs = List.map string_of_int scan_ord in
    print_endline (String.concat " " ("SCAN" :: string_of_int !tot_scan :: scan_strs));

    (* CSCAN *)
    let tot_cscan = ref ((maxc - head) + maxc) in
    if down <> [] then tot_cscan := !tot_cscan + (List.hd (List.rev down));
    let cscan_ord = up @ down in
    let cscan_strs = List.map string_of_int cscan_ord in
    print_endline (String.concat " " ("CSCAN" :: string_of_int !tot_cscan :: cscan_strs));

    (* LOOK *)
    let tot_look = ref 0 in
    if up <> [] && down <> [] then
      tot_look := (List.hd (List.rev up) - head) + (List.hd (List.rev up) - List.hd down)
    else if up <> [] then
      tot_look := List.hd (List.rev up) - head
    else if down <> [] then
      tot_look := head - List.hd down;
    let look_ord = up @ (List.rev down) in
    let look_strs = List.map string_of_int look_ord in
    print_endline (String.concat " " ("LOOK" :: string_of_int !tot_look :: look_strs));

    (* CLOOK *)
    let tot_clook = ref 0 in
    if up <> [] && down <> [] then
      tot_clook := (List.hd (List.rev up) - head) + (List.hd (List.rev up) - List.hd down) + (List.hd (List.rev down) - List.hd down)
    else if up <> [] then
      tot_clook := List.hd (List.rev up) - head
    else if down <> [] then
      tot_clook := (head - List.hd down) + (List.hd (List.rev down) - List.hd down);
    let clook_ord = up @ down in
    let clook_strs = List.map string_of_int clook_ord in
    print_endline (String.concat " " ("CLOOK" :: string_of_int !tot_clook :: clook_strs))
  )
