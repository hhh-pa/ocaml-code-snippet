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
    let g = tokens.(0) in
    let m = tokens.(1) in
    let len_g = String.length g in
    let len_m = String.length m in
    let r = len_g - 1 in
    let d = Array.make (len_m + r) 0 in
    for i = 0 to len_m - 1 do
      d.(i) <- (if m.[i] = '1' then 1 else 0)
    done;
    let g_bits = Array.init len_g (fun j -> if g.[j] = '1' then 1 else 0) in
    for i = 0 to len_m - 1 do
      if d.(i) = 1 then (
        for j = 0 to len_g - 1 do
          d.(i + j) <- d.(i + j) lxor g_bits.(j)
        done
      )
    done;
    let res = Bytes.create r in
    for i = 0 to r - 1 do
      Bytes.set res i (if d.(len_m + i) = 1 then '1' else '0')
    done;
    print_endline (Bytes.to_string res)
  )
