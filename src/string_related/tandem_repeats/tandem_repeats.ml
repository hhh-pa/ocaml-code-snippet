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
    let s = tokens.(0) in
    let n = String.length s in
    let reps = ref [] in
    for i = 0 to n - 1 do
      let max_len = (n - i) / 2 in
      for len = 1 to max_len do
        let sub1 = String.sub s i len in
        let sub2 = String.sub s (i + len) len in
        if sub1 = sub2 then
          reps := Printf.sprintf "%d %d" i len :: !reps
      done
    done;
    let res = List.rev !reps in
    Printf.printf "%d\n" (List.length res);
    List.iter print_endline res
  )
