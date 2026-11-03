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
  if Array.length tokens >= 3 then (
    let p = tokens.(0) in
    let t = tokens.(1) in
    let k = int_of_string tokens.(2) in
    let m = String.length p in
    let n = String.length t in

    let lce i j =
      let ci = ref i in
      let cj = ref j in
      let c = ref 0 in
      while !ci < m && !cj < n && p.[!ci] = t.[!cj] do
        incr ci;
        incr cj;
        incr c
      done;
      !c
    in

    let neg = -1000000000 in
    let off = m + 2 in
    let sz = m + n + 5 in

    let prev = ref (Array.make sz neg) in
    let matched = Array.make n false in

    for d = 0 to k do
      let cur = Array.make sz neg in
      for e = -m to n do
        let r =
          if d = 0 then (
            if e < 0 || e > n then -1
            else 0
          ) else (
            let a = !prev.(e + off) + 1 in
            let b = !prev.(e - 1 + off) in
            let c = !prev.(e + 1 + off) + 1 in
            let mx = max (max a b) c in
            if mx <= neg / 2 then -1 else mx
          )
        in
        if r >= 0 then (
          let l = r + lce r (r + e) in
          cur.(e + off) <- l;
          if l >= m then (
            let end_pos = m + e - 1 in
            if end_pos >= 0 && end_pos < n then
              matched.(end_pos) <- true
          )
        )
      done;
      prev := cur
    done;

    let ans = ref [] in
    for j = 0 to n - 1 do
      if matched.(j) then ans := j :: !ans
    done;
    let res = List.rev !ans in
    Printf.printf "%d\n" (List.length res);
    if res <> [] then (
      let strs = List.map string_of_int res in
      print_endline (String.concat " " strs)
    )
  )
