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
    let d = Hashtbl.create 512 in
    for i = 0 to 255 do
      Hashtbl.add d (String.make 1 (Char.chr i)) i
    done;
    let nxt = ref 256 in
    let w = ref "" in
    let codes = ref [] in
    let n = String.length s in
    for i = 0 to n - 1 do
      let c = String.make 1 s.[i] in
      let wc = !w ^ c in
      if Hashtbl.mem d wc then
        w := wc
      else (
        codes := Hashtbl.find d !w :: !codes;
        Hashtbl.add d wc !nxt;
        incr nxt;
        w := c
      )
    done;
    if !w <> "" then
      codes := Hashtbl.find d !w :: !codes;
    let res = List.rev !codes in
    let strs = List.map string_of_int res in
    print_endline (String.concat " " strs)
  )
