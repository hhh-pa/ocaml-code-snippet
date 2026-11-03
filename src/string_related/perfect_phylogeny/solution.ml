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
    let n = int_of_string tokens.(0) in
    let m = int_of_string tokens.(1) in
    let rows = Array.init n (fun i -> tokens.(i + 2)) in
    let cols = Array.init m (fun j ->
      Array.init n (fun i -> Char.code rows.(i).[j] - Char.code '0')
    ) in
    let possible = ref true in
    let j = ref 0 in
    while !j < m && !possible do
      let col_j = cols.(!j) in
      let k = ref (!j + 1) in
      while !k < m && !possible do
        let col_k = cols.(!k) in
        let h11 = ref false in
        let h10 = ref false in
        let h01 = ref false in
        for i = 0 to n - 1 do
          let a = col_j.(i) in
          let b = col_k.(i) in
          if a = 1 && b = 1 then h11 := true
          else if a = 1 then h10 := true
          else if b = 1 then h01 := true
        done;
        if !h11 && !h10 && !h01 then possible := false;
        incr k
      done;
      incr j
    done;
    if !possible then print_endline "YES"
    else print_endline "NO"
  )
