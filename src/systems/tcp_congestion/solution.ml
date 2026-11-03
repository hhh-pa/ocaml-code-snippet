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
  if Array.length tokens >= 1 then (
    let q = int_of_string tokens.(0) in
    let cwnd = ref 1 in
    let ssthresh = ref 64 in
    let ack_count = ref 0 in
    let idx = ref 1 in
    for _ = 0 to q - 1 do
      let e = tokens.(!idx) in
      incr idx;
      if e = "A" then (
        if !cwnd < !ssthresh then
          incr cwnd
        else (
          incr ack_count;
          if !ack_count >= !cwnd then (
            incr cwnd;
            ack_count := 0
          )
        )
      ) else if e = "D" then (
        let s = !cwnd / 2 in
        ssthresh := (if s < 1 then 1 else s);
        cwnd := !ssthresh;
        ack_count := 0
      ) else if e = "T" then (
        let s = !cwnd / 2 in
        ssthresh := (if s < 1 then 1 else s);
        cwnd := 1;
        ack_count := 0
      );
      Printf.printf "%d %d\n" !cwnd !ssthresh
    done
  )
