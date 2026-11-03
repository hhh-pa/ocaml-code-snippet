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
    let buf = Buffer.create (2 * n + 3) in
    Buffer.add_string buf "^#";
    for i = 0 to n - 1 do
      Buffer.add_char buf s.[i];
      Buffer.add_char buf '#'
    done;
    Buffer.add_char buf '$';
    let t = Buffer.contents buf in
    let m = String.length t in
    let p = Array.make m 0 in
    let c = ref 0 in
    let r = ref 0 in
    for i = 1 to m - 2 do
      let i_mirror = 2 * !c - i in
      if i < !r then
        p.(i) <- min (!r - i) p.(i_mirror);
      while t.[i + 1 + p.(i)] = t.[i - 1 - p.(i)] do
        p.(i) <- p.(i) + 1
      done;
      if i + p.(i) > !r then (
        c := i;
        r := i + p.(i)
      )
    done;
    let max_len = ref 0 in
    let center_idx = ref 0 in
    let total_palindromes = ref 0 in
    for i = 1 to m - 2 do
      if p.(i) > !max_len then (
        max_len := p.(i);
        center_idx := i
      );
      total_palindromes := !total_palindromes + (p.(i) + 1) / 2
    done;
    let start_pos = (!center_idx - !max_len) / 2 in
    let longest_pal = String.sub s start_pos !max_len in
    Printf.printf "%d\n%s\n%d\n" !max_len longest_pal !total_palindromes
  )
