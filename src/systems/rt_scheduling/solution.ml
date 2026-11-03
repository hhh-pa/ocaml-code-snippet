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

let npow2inv n =
  let x = ref 1.0 in
  let fn = float_of_int n in
  for _ = 1 to 200 do
    let p = ref 1.0 in
    for _ = 1 to n - 1 do
      p := !p *. !x
    done;
    let f = !p *. !x -. 2.0 in
    let d = fn *. !p in
    x := !x -. f /. d
  done;
  !x

let () =
  let tokens = read_all_tokens () in
  if Array.length tokens >= 1 then (
    let n = int_of_string tokens.(0) in
    let u = ref 0.0 in
    let idx = ref 1 in
    for _ = 0 to n - 1 do
      let c = float_of_string tokens.(!idx) in
      let t = float_of_string tokens.(!idx + 1) in
      idx := !idx + 2;
      u := !u +. (c /. t)
    done;
    let bound = float_of_int n *. (npow2inv n -. 1.0) in
    if !u <= bound then print_endline "RMS YES"
    else print_endline "RMS NO";
    if !u <= 1.0 then print_endline "EDF YES"
    else print_endline "EDF NO"
  )
