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
    for _ = 0 to q - 1 do
      let k = int_of_string tokens.(!idx) in
      incr idx;
      let tot = ref 0 in
      let wins = ref [] in
      for j = 1 to k do
        let d = int_of_string tokens.(!idx) in
        incr idx;
        let e = min j 10 in
        let win = 1 lsl e in
        wins := win :: !wins;
        let dd = abs d in
        tot := !tot + (dd mod win)
      done;
      let win_strs = List.map string_of_int (List.rev !wins) in
      let out_list = (string_of_int !tot) :: win_strs in
      print_endline (String.concat " " out_list)
    done
  )
