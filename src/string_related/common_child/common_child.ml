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
  if Array.length tokens >= 2 then (
    let a = tokens.(0) in
    let b = tokens.(1) in
    let n = String.length a in
    let m = String.length b in
    let prev = ref (Array.make (m + 1) 0) in
    let curr = ref (Array.make (m + 1) 0) in
    for i = 1 to n do
      let a_char = a.[i - 1] in
      let p = !prev in
      let c = !curr in
      for j = 1 to m do
        if a_char = b.[j - 1] then
          c.(j) <- p.(j - 1) + 1
        else
          c.(j) <- max p.(j) c.(j - 1)
      done;
      let tmp = !prev in
      prev := !curr;
      curr := tmp
    done;
    Printf.printf "%d\n" (!prev).(m)
  )
