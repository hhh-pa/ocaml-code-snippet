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

let build_sa s =
  let n = String.length s in
  let sa = Array.init n (fun i -> i) in
  let rnk = ref (Array.init n (fun i -> Char.code s.[i])) in
  let tmp = ref (Array.make n 0) in
  let k = ref 1 in
  while !k < n do
    let cur_k = !k in
    let r = !rnk in
    let cmp a b =
      let ra = r.(a) in
      let rb = r.(b) in
      if ra <> rb then compare ra rb
      else
        let ra2 = if a + cur_k < n then r.(a + cur_k) + 1 else 0 in
        let rb2 = if b + cur_k < n then r.(b + cur_k) + 1 else 0 in
        compare ra2 rb2
    in
    Array.sort cmp sa;
    !tmp.(sa.(0)) <- 0;
    for i = 1 to n - 1 do
      let prev = sa.(i - 1) in
      let curr = sa.(i) in
      let diff =
        r.(prev) <> r.(curr) ||
        (if prev + cur_k < n then r.(prev + cur_k) + 1 else 0) <>
        (if curr + cur_k < n then r.(curr + cur_k) + 1 else 0)
      in
      !tmp.(curr) <- !tmp.(prev) + (if diff then 1 else 0)
    done;
    let t = !rnk in
    rnk := !tmp;
    tmp := t;
    if !rnk.(sa.(n - 1)) = n - 1 then k := n
    else k := !k * 2
  done;
  sa

let build_lcp s sa =
  let n = String.length s in
  let rnk = Array.make n 0 in
  for i = 0 to n - 1 do
    rnk.(sa.(i)) <- i
  done;
  let lcp = Array.make n 0 in
  let h = ref 0 in
  for i = 0 to n - 1 do
    if rnk.(i) > 0 then (
      let j = sa.(rnk.(i) - 1) in
      while i + !h < n && j + !h < n && s.[i + !h] = s.[j + !h] do
        incr h
      done;
      lcp.(rnk.(i)) <- !h;
      if !h > 0 then decr h
    )
  done;
  lcp

let () =
  let tokens = read_all_tokens () in
  if Array.length tokens >= 1 then (
    let s = tokens.(0) in
    let n = String.length s in
    if n = 0 then (
      Printf.printf "\n\n"
    ) else (
      let sa = build_sa s in
      let lcp = build_lcp s sa in
      let sa_strs = Array.to_list (Array.map string_of_int sa) in
      let lcp_strs = Array.to_list (Array.map string_of_int lcp) in
      print_endline (String.concat " " sa_strs);
      print_endline (String.concat " " lcp_strs)
    )
  )
