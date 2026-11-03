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
    let r = int_of_string tokens.(1) in
    let idx = ref 2 in
    let roots = Array.init r (fun _ ->
      let v = int_of_string tokens.(!idx) in
      incr idx;
      v
    ) in
    let refs = Array.init n (fun _ ->
      let k = int_of_string tokens.(!idx) in
      incr idx;
      let neighbors = Array.init k (fun _ ->
        let v = int_of_string tokens.(!idx) in
        incr idx;
        v
      ) in
      Array.to_list neighbors
    ) in

    let marked = Array.make n false in
    let stack = ref [] in
    for i = 0 to r - 1 do
      let rt = roots.(i) in
      if rt >= 0 && rt < n && not marked.(rt) then (
        marked.(rt) <- true;
        stack := rt :: !stack;
        while !stack <> [] do
          let u = List.hd !stack in
          stack := List.tl !stack;
          List.iter (fun v ->
            if v >= 0 && v < n && not marked.(v) then (
              marked.(v) <- true;
              stack := v :: !stack
            )
          ) refs.(u)
        done
      )
    done;

    let freed = ref [] in
    for i = 0 to n - 1 do
      if not marked.(i) then freed := i :: !freed
    done;
    let res = List.rev !freed in
    if res = [] then print_endline "NONE"
    else (
      let strs = List.map string_of_int res in
      print_endline (String.concat " " strs)
    )
  )
