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

module IntSet = Set.Make(Int)

let () =
  let tokens = read_all_tokens () in
  if Array.length tokens >= 3 then (
    let n = int_of_string tokens.(0) in
    let m = int_of_string tokens.(1) in
    let k = int_of_string tokens.(2) in
    let idx = ref 3 in

    let adj = Array.init n (fun _ -> ref IntSet.empty) in
    for _ = 1 to m do
      if !idx + 1 < Array.length tokens then (
        let u = int_of_string tokens.(!idx) in
        let v = int_of_string tokens.(!idx + 1) in
        idx := !idx + 2;
        adj.(u) := IntSet.add v !(adj.(u));
        adj.(v) := IntSet.add u !(adj.(v))
      )
    done;

    let active = ref IntSet.empty in
    for i = 0 to n - 1 do
      active := IntSet.add i !active
    done;

    let select_stack = Stack.create () in
    let spilled = Array.make n false in

    while not (IntSet.is_empty !active) do
      let simplify_node = ref (-1) in
      let elements = IntSet.elements !active in
      let it = ref elements in
      while !it <> [] && !simplify_node = -1 do
        let u = List.hd !it in
        it := List.tl !it;
        let deg = ref 0 in
        IntSet.iter (fun v ->
          if IntSet.mem v !active then incr deg
        ) !(adj.(u));
        if !deg < k then simplify_node := u
      done;

      if !simplify_node <> -1 then (
        active := IntSet.remove !simplify_node !active;
        Stack.push !simplify_node select_stack
      ) else (
        let spill_node = IntSet.min_elt !active in
        active := IntSet.remove spill_node !active;
        spilled.(spill_node) <- true
      )
    done;

    let color = Array.make n (-1) in
    while not (Stack.is_empty select_stack) do
      let u = Stack.pop select_stack in
      let used_colors = ref IntSet.empty in
      IntSet.iter (fun v ->
        if color.(v) <> -1 then
          used_colors := IntSet.add color.(v) !used_colors
      ) !(adj.(u));

      let c = ref 0 in
      let found_color = ref false in
      while !c < k && not !found_color do
        if not (IntSet.mem !c !used_colors) then (
          color.(u) <- !c;
          found_color := true
        );
        incr c
      done;
      if color.(u) = -1 then spilled.(u) <- true
    done;

    for i = 0 to n - 1 do
      if spilled.(i) then
        Printf.printf "v%d:SPILL\n" i
      else
        Printf.printf "v%d:R%d\n" i color.(i)
    done
  )
