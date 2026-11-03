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
    let idx = ref 1 in
    let srtt = ref 0 in
    let rttvar = ref 0 in
    let rto = ref 1000 in
    let have_sample = ref false in

    for _ = 0 to q - 1 do
      let e = tokens.(!idx) in
      incr idx;
      if e = "S" then (
        let r = int_of_string tokens.(!idx) in
        let ret = int_of_string tokens.(!idx + 1) in
        idx := !idx + 2;
        if ret = 0 then (
          if not !have_sample then (
            have_sample := true;
            srtt := r;
            rttvar := r / 2
          ) else (
            rttvar := (3 * !rttvar + abs (!srtt - r)) / 4;
            srtt := (7 * !srtt + r) / 8
          );
          let x = 4 * !rttvar in
          let x = if x < 1 then 1 else x in
          let cur_rto = !srtt + x in
          let cur_rto = if cur_rto < 1 then 1 else cur_rto in
          let cur_rto = if cur_rto > 60000 then 60000 else cur_rto in
          rto := cur_rto
        )
      ) else if e = "T" then (
        rto := !rto * 2;
        if !rto > 60000 then rto := 60000
      );
      Printf.printf "%d %d %d\n" !srtt !rttvar !rto
    done
  )
