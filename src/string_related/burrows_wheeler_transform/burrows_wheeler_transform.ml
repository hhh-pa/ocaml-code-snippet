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
    let t = s ^ "$" in
    let n = String.length t in
    let sa = Array.init n (fun i -> i) in
    Array.sort (fun a b ->
      let len_a = n - a in
      let len_b = n - b in
      let rec cmp i =
        if i = len_a && i = len_b then 0
        else if i = len_a then -1
        else if i = len_b then 1
        else
          let ca = t.[a + i] in
          let cb = t.[b + i] in
          if ca < cb then -1
          else if ca > cb then 1
          else cmp (i + 1)
      in
      cmp 0
    ) sa;
    let bwt = Bytes.create n in
    for i = 0 to n - 1 do
      Bytes.set bwt i t.[(sa.(i) - 1 + n) mod n]
    done;
    print_endline (Bytes.to_string bwt)
  )
