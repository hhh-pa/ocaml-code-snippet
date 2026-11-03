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

let solve s =
  let p = ref 0 in
  let err = ref false in
  let len = String.length s in

  let rec parse_f () =
    if !err || !p >= len then ( err := true; 0L )
    else if s.[!p] = '(' then (
      incr p;
      let v = parse_e () in
      if !err || !p >= len || s.[!p] <> ')' then ( err := true; 0L )
      else ( incr p; v )
    ) else if s.[!p] = '-' then (
      incr p;
      Int64.neg (parse_f ())
    ) else if s.[!p] < '0' || s.[!p] > '9' then (
      err := true; 0L
    ) else (
      let v = ref 0L in
      while !p < len && s.[!p] >= '0' && s.[!p] <= '9' do
        let digit = Int64.of_int (Char.code s.[!p] - Char.code '0') in
        v := Int64.add (Int64.mul !v 10L) digit;
        incr p
      done;
      !v
    )

  and parse_t () =
    let v = ref (parse_f ()) in
    while not !err && !p < len && (s.[!p] = '*' || s.[!p] = '/') do
      let op = s.[!p] in
      incr p;
      let r = parse_f () in
      if not !err then (
        if op = '*' then
          v := Int64.mul !v r
        else (
          if r = 0L then (
            err := true
          ) else (
            v := Int64.div !v r
          )
        )
      )
    done;
    !v

  and parse_e () =
    let v = ref (parse_t ()) in
    while not !err && !p < len && (s.[!p] = '+' || s.[!p] = '-') do
      let op = s.[!p] in
      incr p;
      let r = parse_t () in
      if not !err then (
        if op = '+' then
          v := Int64.add !v r
        else
          v := Int64.sub !v r
      )
    done;
    !v
  in

  let v = parse_e () in
  if !err || !p <> len then
    Printf.printf "RD ERROR\n"
  else
    Printf.printf "RD %Ld\n" v

let () =
  let tokens = read_all_tokens () in
  if Array.length tokens >= 1 then (
    let t = int_of_string tokens.(0) in
    for i = 1 to t do
      if i < Array.length tokens then
        solve tokens.(i)
    done
  )
