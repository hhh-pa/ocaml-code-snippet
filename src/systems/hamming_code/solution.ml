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

let npar k =
  let p = ref 0 in
  while (1 lsl !p) < k + !p + 1 do
    incr p
  done;
  !p

let ispow2 x = x > 0 && (x land (x - 1)) = 0

let encode data =
  let k = String.length data in
  let p = npar k in
  let hlen = k + p in
  let h = Array.make (hlen + 1) 0 in
  let di = ref 0 in
  for i = 1 to hlen do
    if not (ispow2 i) then (
      h.(i) <- (if data.[!di] = '1' then 1 else 0);
      incr di
    )
  done;
  for b = 0 to p - 1 do
    let s = ref 0 in
    let mask = 1 lsl b in
    for i = 1 to hlen do
      if (i land mask) <> 0 then s := !s lxor h.(i)
    done;
    h.(mask) <- !s
  done;
  let p0 = ref 0 in
  for i = 1 to hlen do
    p0 := !p0 lxor h.(i)
  done;
  let res = Bytes.create (hlen + 1) in
  Bytes.set res 0 (if !p0 = 1 then '1' else '0');
  for i = 1 to hlen do
    Bytes.set res i (if h.(i) = 1 then '1' else '0')
  done;
  Bytes.to_string res

let decode code =
  let hlen = String.length code - 1 in
  let h = Array.make (hlen + 1) 0 in
  let p0 = ref (if code.[0] = '1' then 1 else 0) in
  for i = 1 to hlen do
    h.(i) <- (if code.[i] = '1' then 1 else 0)
  done;
  let p = ref 0 in
  while (1 lsl !p) <= hlen do
    incr p
  done;
  let syn = ref 0 in
  for b = 0 to !p - 1 do
    let s = ref 0 in
    let mask = 1 lsl b in
    for i = 1 to hlen do
      if (i land mask) <> 0 then s := !s lxor h.(i)
    done;
    if !s <> 0 then syn := !syn lor mask
  done;
  let tot = ref !p0 in
  for i = 1 to hlen do
    tot := !tot lxor h.(i)
  done;
  let status =
    if !syn = 0 && !tot = 0 then "OK"
    else if !syn = 0 then (
      p0 := !p0 lxor 1;
      "FIXED"
    ) else if !tot <> 0 then (
      if !syn <= hlen then h.(!syn) <- h.(!syn) lxor 1;
      "FIXED"
    ) else "DED"
  in
  let buf = Buffer.create hlen in
  for i = 1 to hlen do
    if not (ispow2 i) then
      Buffer.add_char buf (if h.(i) = 1 then '1' else '0')
  done;
  (Buffer.contents buf, status)

let () =
  let tokens = read_all_tokens () in
  if Array.length tokens >= 1 then (
    let q = int_of_string tokens.(0) in
    let idx = ref 1 in
    for _ = 0 to q - 1 do
      let op = tokens.(!idx) in
      let w = tokens.(!idx + 1) in
      idx := !idx + 2;
      if op = "E" then print_endline (encode w)
      else (
        let (d, s) = decode w in
        Printf.printf "%s %s\n" d s
      )
    done
  )
