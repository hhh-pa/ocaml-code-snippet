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

let timeout = 4

let () =
  let tokens = read_all_tokens () in
  if Array.length tokens >= 4 then (
    let policy = int_of_string tokens.(0) in
    let w_in = int_of_string tokens.(1) in
    let w = if policy = 0 then 1 else w_in in
    let n = int_of_string tokens.(2) in
    let l = int_of_string tokens.(3) in
    let idx = ref 4 in

    let floss = Hashtbl.create 16 in
    let aloss = Hashtbl.create 16 in
    for _ = 0 to l - 1 do
      let t = tokens.(!idx) in
      let s = int_of_string tokens.(!idx + 1) in
      idx := !idx + 2;
      let tbl = if t = "F" then floss else aloss in
      let cur = match Hashtbl.find_opt tbl s with Some v -> v | None -> 0 in
      Hashtbl.replace tbl s (cur + 1)
    done;

    let base = ref 0 in
    let next_to_send = ref 0 in
    let sent = Array.make n (-1000000) in
    let acked = Array.make n 0 in
    let expected = ref 0 in
    let buffered = Array.make n 0 in
    let farr = Hashtbl.create 64 in
    let aarr = Hashtbl.create 64 in
    let frame_tx = ref 0 in
    let ack_tx = ref 0 in

    let send_frame j now =
      incr frame_tx;
      sent.(j) <- now;
      let cur_fl = match Hashtbl.find_opt floss j with Some v -> v | None -> 0 in
      if cur_fl > 0 then
        Hashtbl.replace floss j (cur_fl - 1)
      else
        let t = now + 1 in
        let cur = match Hashtbl.find_opt farr t with Some lst -> lst | None -> [] in
        Hashtbl.replace farr t (j :: cur)
    in

    let send_ack j now =
      incr ack_tx;
      let cur_al = match Hashtbl.find_opt aloss j with Some v -> v | None -> 0 in
      if cur_al > 0 then
        Hashtbl.replace aloss j (cur_al - 1)
      else
        let t = now + 1 in
        let cur = match Hashtbl.find_opt aarr t with Some lst -> lst | None -> [] in
        Hashtbl.replace aarr t (j :: cur)
    in

    let sim_done = ref false in
    let now = ref 0 in
    while !now < 90000 && not !sim_done do
      (match Hashtbl.find_opt farr !now with
      | Some lst ->
        let frames = List.rev lst in
        Hashtbl.remove farr !now;
        List.iter (fun j ->
          if j >= 0 && j < n then (
            if policy = 2 then (
              if j = !expected then (
                incr expected;
                while !expected < n && buffered.(!expected) <> 0 do
                  buffered.(!expected) <- 0;
                  incr expected
                done
              ) else if j > !expected && j < !expected + w then (
                buffered.(j) <- 1
              );
              send_ack j !now
            ) else (
              if j = !expected then incr expected;
              if !expected > 0 then send_ack (!expected - 1) !now
            )
          )
        ) frames
      | None -> ());

      (match Hashtbl.find_opt aarr !now with
      | Some lst ->
        let acks = List.rev lst in
        Hashtbl.remove aarr !now;
        List.iter (fun j ->
          if policy = 2 then (
            if j >= 0 && j < n then acked.(j) <- 1
          ) else (
            let upper = min j (n - 1) in
            for k = !base to upper do
              acked.(k) <- 1
            done
          )
        ) acks
      | None -> ());

      while !base < n && acked.(!base) <> 0 do
        incr base
      done;

      if !base >= n then (
        sim_done := true
      ) else (
        if policy = 2 then (
          for j = !base to !next_to_send - 1 do
            if acked.(j) = 0 && !now - sent.(j) >= timeout then
              send_frame j !now
          done
        ) else (
          if !base < !next_to_send && !now - sent.(!base) >= timeout then
            next_to_send := !base
        );

        while !next_to_send < !base + w && !next_to_send < n do
          send_frame !next_to_send !now;
          incr next_to_send
        done
      );
      incr now
    done;

    Printf.printf "%d %d\n" !frame_tx !ack_tx
  )
