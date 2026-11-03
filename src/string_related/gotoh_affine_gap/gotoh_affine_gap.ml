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
    let s1 = tokens.(0) in
    let s2 = tokens.(1) in
    let n = String.length s1 in
    let m = String.length s2 in

    let neg = -1000000000 in
    let match_sc = 2 in
    let mismatch = -1 in
    let gap_open = -2 in
    let gap_extend = -1 in

    let m_mat = Array.make_matrix (n + 1) (m + 1) neg in
    let ix_mat = Array.make_matrix (n + 1) (m + 1) neg in
    let iy_mat = Array.make_matrix (n + 1) (m + 1) neg in

    m_mat.(0).(0) <- 0;
    for i = 1 to n do
      ix_mat.(i).(0) <- gap_open + (i - 1) * gap_extend
    done;
    for j = 1 to m do
      iy_mat.(0).(j) <- gap_open + (j - 1) * gap_extend
    done;

    for i = 1 to n do
      let s1_char = s1.[i - 1] in
      for j = 1 to m do
        let sc = if s1_char = s2.[j - 1] then match_sc else mismatch in
        let best_diag = max (max m_mat.(i - 1).(j - 1) ix_mat.(i - 1).(j - 1)) iy_mat.(i - 1).(j - 1) in
        m_mat.(i).(j) <- best_diag + sc;
        ix_mat.(i).(j) <- max (m_mat.(i - 1).(j) + gap_open) (ix_mat.(i - 1).(j) + gap_extend);
        iy_mat.(i).(j) <- max (m_mat.(i).(j - 1) + gap_open) (iy_mat.(i).(j - 1) + gap_extend)
      done
    done;

    let ans = max (max m_mat.(n).(m) ix_mat.(n).(m)) iy_mat.(n).(m) in
    Printf.printf "%d\n" ans
  )
