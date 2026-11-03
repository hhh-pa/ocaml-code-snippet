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

let match_sc = 2
let mismatch = -1
let gap = -2

let rev_string s =
  let len = String.length s in
  let b = Bytes.create len in
  for i = 0 to len - 1 do
    Bytes.set b i s.[len - 1 - i]
  done;
  Bytes.to_string b

let solve_global s1 s2 =
  let n = String.length s1 in
  let m = String.length s2 in
  let dp = Array.make_matrix (n + 1) (m + 1) 0 in
  for i = 1 to n do dp.(i).(0) <- dp.(i - 1).(0) + gap done;
  for j = 1 to m do dp.(0).(j) <- dp.(0).(j - 1) + gap done;
  for i = 1 to n do
    let c1 = s1.[i - 1] in
    for j = 1 to m do
      let diag = dp.(i - 1).(j - 1) + (if c1 = s2.[j - 1] then match_sc else mismatch) in
      let up = dp.(i - 1).(j) + gap in
      let left = dp.(i).(j - 1) + gap in
      dp.(i).(j) <- max (max diag up) left
    done
  done;
  let a1 = Buffer.create (n + m) in
  let a2 = Buffer.create (n + m) in
  let i = ref n in
  let j = ref m in
  while !i > 0 || !j > 0 do
    if !i > 0 && !j > 0 && dp.(!i).(!j) = dp.(!i - 1).(!j - 1) + (if s1.[!i - 1] = s2.[!j - 1] then match_sc else mismatch) then (
      Buffer.add_char a1 s1.[!i - 1];
      Buffer.add_char a2 s2.[!j - 1];
      decr i;
      decr j
    ) else if !i > 0 && dp.(!i).(!j) = dp.(!i - 1).(!j) + gap then (
      Buffer.add_char a1 s1.[!i - 1];
      Buffer.add_char a2 '-';
      decr i
    ) else (
      Buffer.add_char a1 '-';
      Buffer.add_char a2 s2.[!j - 1];
      decr j
    )
  done;
  Printf.printf "%d\n%s\n%s\n" dp.(n).(m) (rev_string (Buffer.contents a1)) (rev_string (Buffer.contents a2))

let solve_local s1 s2 =
  let n = String.length s1 in
  let m = String.length s2 in
  let dp = Array.make_matrix (n + 1) (m + 1) 0 in
  let max_score = ref 0 in
  let max_i = ref 0 in
  let max_j = ref 0 in
  for i = 1 to n do
    let c1 = s1.[i - 1] in
    for j = 1 to m do
      let diag = dp.(i - 1).(j - 1) + (if c1 = s2.[j - 1] then match_sc else mismatch) in
      let up = dp.(i - 1).(j) + gap in
      let left = dp.(i).(j - 1) + gap in
      let score = max 0 (max (max diag up) left) in
      dp.(i).(j) <- score;
      if score > !max_score then (
        max_score := score;
        max_i := i;
        max_j := j
      )
    done
  done;
  let a1 = Buffer.create (n + m) in
  let a2 = Buffer.create (n + m) in
  let i = ref !max_i in
  let j = ref !max_j in
  while !i > 0 && !j > 0 && dp.(!i).(!j) > 0 do
    if dp.(!i).(!j) = dp.(!i - 1).(!j - 1) + (if s1.[!i - 1] = s2.[!j - 1] then match_sc else mismatch) then (
      Buffer.add_char a1 s1.[!i - 1];
      Buffer.add_char a2 s2.[!j - 1];
      decr i;
      decr j
    ) else if dp.(!i).(!j) = dp.(!i - 1).(!j) + gap then (
      Buffer.add_char a1 s1.[!i - 1];
      Buffer.add_char a2 '-';
      decr i
    ) else (
      Buffer.add_char a1 '-';
      Buffer.add_char a2 s2.[!j - 1];
      decr j
    )
  done;
  Printf.printf "%d\n%s\n%s\n" !max_score (rev_string (Buffer.contents a1)) (rev_string (Buffer.contents a2))

let () =
  let tokens = read_all_tokens () in
  if Array.length tokens >= 3 then (
    let mode = tokens.(0) in
    let s1 = tokens.(1) in
    let s2 = tokens.(2) in
    if mode = "GLOBAL" then solve_global s1 s2
    else solve_local s1 s2
  )
