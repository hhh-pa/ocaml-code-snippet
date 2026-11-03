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

let z_function text pattern =
  let m = String.length pattern in
  if m = 0 then true
  else (
    let s = pattern ^ "$" ^ text in
    let n = String.length s in
    let z = Array.make n 0 in
    let l = ref 0 in
    let r = ref 0 in
    let found = ref false in
    let i = ref 1 in
    while !i < n && not !found do
      if !i <= !r then
        z.(!i) <- min (!r - !i + 1) z.(!i - !l);
      while !i + z.(!i) < n && s.[z.(!i)] = s.[!i + z.(!i)] do
        z.(!i) <- z.(!i) + 1
      done;
      if !i + z.(!i) - 1 > !r then (
        l := !i;
        r := !i + z.(!i) - 1
      );
      if z.(!i) = m then found := true;
      incr i
    done;
    !found
  )

let () =
  let tokens = read_all_tokens () in
  if Array.length tokens >= 1 then (
    let n = int_of_string tokens.(0) in
    let idx = ref 1 in
    for _ = 0 to n - 1 do
      let text = tokens.(!idx) in
      let pattern = tokens.(!idx + 1) in
      idx := !idx + 2;
      if z_function text pattern then
        print_endline "YES"
      else
        print_endline "NO"
    done
  )
