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
    let a = tokens.(0) in
    let b = tokens.(1) in
    let n = String.length a in
    let m = String.length b in

    let dp = Array.make_matrix (n + 1) (m + 1) 0 in
    for i = 0 to n do dp.(i).(0) <- i done;
    for j = 0 to m do dp.(0).(j) <- j done;

    let memo = Hashtbl.create 256 in

    let solve_block a0 a1 b0 b1 t0 t1 l0 l1 =
      let k = (a0, a1, b0, b1, t0, t1, l0, l1) in
      match Hashtbl.find_opt memo k with
      | Some v -> v
      | None ->
        let _t0_val = 0 in
        let t1_val = t0 in
        let t2_val = t0 + t1 in
        let l0_val = 0 in
        let l1_val = l0 in
        let l2_val = l0 + l1 in
        let x11 = min (min (t1_val + 1) (l1_val + 1)) (l0_val + (if a0 = b0 then 0 else 1)) in
        let x12 = min (min (t2_val + 1) (x11 + 1)) (t1_val + (if a0 = b1 then 0 else 1)) in
        let x21 = min (min (x11 + 1) (l2_val + 1)) (l1_val + (if a1 = b0 then 0 else 1)) in
        let x22 = min (min (x12 + 1) (x21 + 1)) (x11 + (if a1 = b1 then 0 else 1)) in
        let v = (x21 - l2_val, x22 - x21, x12 - t2_val, x22 - x12) in
        Hashtbl.add memo k v;
        v
    in

    let cell i j =
      let c = if a.[i - 1] = b.[j - 1] then 0 else 1 in
      min (min (dp.(i - 1).(j) + 1) (dp.(i).(j - 1) + 1)) (dp.(i - 1).(j - 1) + c)
    in

    let nb = n / 2 in
    let mb = m / 2 in

    for bi = 0 to nb - 1 do
      for bj = 0 to mb - 1 do
        let i0 = 2 * bi in
        let j0 = 2 * bj in
        let t0 = dp.(i0).(j0 + 1) - dp.(i0).(j0) in
        let t1 = dp.(i0).(j0 + 2) - dp.(i0).(j0 + 1) in
        let l0 = dp.(i0 + 1).(j0) - dp.(i0).(j0) in
        let l1 = dp.(i0 + 2).(j0) - dp.(i0 + 1).(j0) in
        let (v0, v1, v2, _v3) = solve_block a.[i0] a.[i0 + 1] b.[j0] b.[j0 + 1] t0 t1 l0 l1 in
        dp.(i0 + 2).(j0 + 1) <- dp.(i0 + 2).(j0) + v0;
        dp.(i0 + 2).(j0 + 2) <- dp.(i0 + 2).(j0 + 1) + v1;
        dp.(i0 + 1).(j0 + 2) <- dp.(i0).(j0 + 2) + v2
      done
    done;

    let nodd = (n mod 2 = 1) in
    let modd = (m mod 2 = 1) in
    if nodd && modd then (
      for j = 1 to m - 1 do
        dp.(n).(j) <- cell n j
      done;
      for i = 1 to n do
        dp.(i).(m) <- cell i m
      done
    ) else (
      if modd then
        for i = 1 to n do
          dp.(i).(m) <- cell i m
        done;
      if nodd then
        for j = 1 to m do
          dp.(n).(j) <- cell n j
        done
    );

    Printf.printf "%d\n" dp.(n).(m)
  )
