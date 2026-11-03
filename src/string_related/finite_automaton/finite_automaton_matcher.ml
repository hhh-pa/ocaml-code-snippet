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

let compute_transition_function pattern =
  let m = String.length pattern in
  let delta = Array.make_matrix (m + 1) 26 0 in
  let pi = Array.make m 0 in
  for i = 1 to m - 1 do
    let j = ref pi.(i - 1) in
    while !j > 0 && pattern.[i] <> pattern.[!j] do
      j := pi.(!j - 1)
    done;
    if pattern.[i] = pattern.[!j] then incr j;
    pi.(i) <- !j
  done;
  for q = 0 to m do
    for c = 0 to 25 do
      let ch = Char.chr (Char.code 'a' + c) in
      if q < m && pattern.[q] = ch then
        delta.(q).(c) <- q + 1
      else if q = 0 then
        delta.(q).(c) <- 0
      else
        delta.(q).(c) <- delta.(pi.(q - 1)).(c)
    done
  done;
  delta

let finite_automaton_matcher pattern text =
  let m = String.length pattern in
  let n = String.length text in
  let delta = compute_transition_function pattern in
  let matches = ref [] in
  let q = ref 0 in
  for i = 0 to n - 1 do
    let c = Char.code text.[i] - Char.code 'a' in
    q := delta.(!q).(c);
    if !q = m then
      matches := (i + 1 - m) :: !matches
  done;
  List.rev !matches

let () =
  let tokens = read_all_tokens () in
  if Array.length tokens >= 1 then (
    let num_q = int_of_string tokens.(0) in
    let idx = ref 1 in
    for _ = 0 to num_q - 1 do
      let pattern = tokens.(!idx) in
      let text = tokens.(!idx + 1) in
      idx := !idx + 2;
      let matches = finite_automaton_matcher pattern text in
      if matches = [] then
        print_endline "-1"
      else (
        let strs = List.map string_of_int matches in
        print_endline (String.concat " " strs)
      )
    done
  )
