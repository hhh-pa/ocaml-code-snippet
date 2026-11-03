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
  if Array.length tokens > 0 then (
    let t = int_of_string tokens.(0) in
    let idx = ref 1 in
    for _ = 1 to t do
      if !idx + 1 < Array.length tokens then (
        let sg = tokens.(!idx) in
        let bs = tokens.(!idx + 1) in
        idx := !idx + 2;
        let v = ref 0L in
        for j = 0 to String.length bs - 1 do
          let bit = if bs.[j] = '1' then 1L else 0L in
          v := Int64.add (Int64.mul !v 2L) bit
        done;
        if sg = "-" then v := Int64.neg !v;
        Printf.printf "ATTR %Ld\n" !v
      )
    done
  )
