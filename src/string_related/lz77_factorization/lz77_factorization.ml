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
    let i = ref 0 in
    let out = ref [] in
    while !i < n do
      let best_len = ref 0 in
      let best_off = ref 0 in
      for j = 0 to !i - 1 do
        let len = ref 0 in
        while !i + !len < n && j + !len < n && s.[j + !len] = s.[!i + !len] do
          incr len
        done;
        if !len > !best_len then (
          best_len := !len;
          best_off := !i - j
        )
      done;
      if !best_len = 0 then (
        out := Printf.sprintf "0 0 %c" s.[!i] :: !out;
        incr i
      ) else (
        let nxt = if !i + !best_len < n then s.[!i + !best_len] else '$' in
        out := Printf.sprintf "%d %d %c" !best_off !best_len nxt :: !out;
        i := !i + !best_len + (if !i + !best_len < n then 1 else 0);
        if !best_len > 0 && !i >= n then i := n + 1
      )
    done;
    let lines = List.rev !out in
    List.iter print_endline lines
  )
