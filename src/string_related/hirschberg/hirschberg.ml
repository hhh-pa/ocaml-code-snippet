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

let nw_score x y =
  let nx = String.length x in
  let ny = String.length y in
  let prev = Array.make (ny + 1) 0 in
  let curr = Array.make (ny + 1) 0 in
  for j = 1 to ny do
    prev.(j) <- prev.(j - 1) + gap
  done;
  for i = 1 to nx do
    curr.(0) <- prev.(0) + gap;
    let xi = x.[i - 1] in
    for j = 1 to ny do
      let diag = prev.(j - 1) + (if xi = y.[j - 1] then match_sc else mismatch) in
      let up = prev.(j) + gap in
      let left = curr.(j - 1) + gap in
      curr.(j) <- max (max diag up) left
    done;
    Array.blit curr 0 prev 0 (ny + 1)
  done;
  prev

let nw_align x y =
  let n = String.length x in
  let m = String.length y in
  let dp = Array.make_matrix (n + 1) (m + 1) 0 in
  for i = 1 to n do dp.(i).(0) <- dp.(i - 1).(0) + gap done;
  for j = 1 to m do dp.(0).(j) <- dp.(0).(j - 1) + gap done;
  for i = 1 to n do
    let xi = x.[i - 1] in
    for j = 1 to m do
      let diag = dp.(i - 1).(j - 1) + (if xi = y.[j - 1] then match_sc else mismatch) in
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
    if !i > 0 && !j > 0 && dp.(!i).(!j) = dp.(!i - 1).(!j - 1) + (if x.[!i - 1] = y.[!j - 1] then match_sc else mismatch) then (
      Buffer.add_char a1 x.[!i - 1];
      Buffer.add_char a2 y.[!j - 1];
      decr i;
      decr j
    ) else if !i > 0 && dp.(!i).(!j) = dp.(!i - 1).(!j) + gap then (
      Buffer.add_char a1 x.[!i - 1];
      Buffer.add_char a2 '-';
      decr i
    ) else (
      Buffer.add_char a1 '-';
      Buffer.add_char a2 y.[!j - 1];
      decr j
    )
  done;
  (rev_string (Buffer.contents a1), rev_string (Buffer.contents a2))

let rec hirschberg x y =
  let n = String.length x in
  let m = String.length y in
  if n = 0 then (String.make m '-', y)
  else if m = 0 then (x, String.make n '-')
  else if n = 1 || m = 1 then nw_align x y
  else (
    let xmid = n / 2 in
    let x_left = String.sub x 0 xmid in
    let x_right = String.sub x xmid (n - xmid) in
    let score_l = nw_score x_left y in
    let score_r = nw_score (rev_string x_right) (rev_string y) in
    let best_score = ref (-1000000000) in
    let best_j = ref 0 in
    for j = 0 to m do
      let total = score_l.(j) + score_r.(m - j) in
      if total > !best_score then (
        best_score := total;
        best_j := j
      )
    done;
    let (l1, l2) = hirschberg x_left (String.sub y 0 !best_j) in
    let (r1, r2) = hirschberg x_right (String.sub y !best_j (m - !best_j)) in
    (l1 ^ r1, l2 ^ r2)
  )

let () =
  let tokens = read_all_tokens () in
  if Array.length tokens >= 2 then (
    let s1 = tokens.(0) in
    let s2 = tokens.(1) in
    let (a1, a2) = hirschberg s1 s2 in
    let len = String.length a1 in
    let score = ref 0 in
    for i = 0 to len - 1 do
      let c1 = a1.[i] in
      let c2 = a2.[i] in
      if c1 = '-' || c2 = '-' then
        score := !score + gap
      else if c1 = c2 then
        score := !score + match_sc
      else
        score := !score + mismatch
    done;
    Printf.printf "%d\n%s\n%s\n" !score a1 a2
  )
