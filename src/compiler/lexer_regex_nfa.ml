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

let is_alnum c =
  (c >= '0' && c <= '9') || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

let insert_concat regex =
  let b = Buffer.create (String.length regex * 2) in
  let n = String.length regex in
  for i = 0 to n - 1 do
    let c1 = regex.[i] in
    Buffer.add_char b c1;
    if i + 1 < n then (
      let c2 = regex.[i + 1] in
      let c1_can_end = is_alnum c1 || c1 = '*' || c1 = ')' in
      let c2_can_start = is_alnum c2 || c2 = '(' in
      if c1_can_end && c2_can_start then
        Buffer.add_char b '.'
    )
  done;
  Buffer.contents b

let get_prec c =
  match c with
  | '*' -> 3
  | '.' -> 2
  | '|' -> 1
  | _ -> 0

let to_postfix infix =
  let postfix = Buffer.create (String.length infix) in
  let op_stack = Stack.create () in
  for i = 0 to String.length infix - 1 do
    let c = infix.[i] in
    if is_alnum c then
      Buffer.add_char postfix c
    else if c = '(' then
      Stack.push c op_stack
    else if c = ')' then (
      while not (Stack.is_empty op_stack) && Stack.top op_stack <> '(' do
        Buffer.add_char postfix (Stack.pop op_stack)
      done;
      if not (Stack.is_empty op_stack) then ignore (Stack.pop op_stack)
    ) else (
      while not (Stack.is_empty op_stack) && Stack.top op_stack <> '(' && get_prec (Stack.top op_stack) >= get_prec c do
        Buffer.add_char postfix (Stack.pop op_stack)
      done;
      Stack.push c op_stack
    )
  done;
  while not (Stack.is_empty op_stack) do
    Buffer.add_char postfix (Stack.pop op_stack)
  done;
  Buffer.contents postfix

type transition = { ch : char; to_state : int }
type frag = { s_start : int; s_end : int }

module IntSet = Set.Make(Int)

let () =
  let tokens = read_all_tokens () in
  if Array.length tokens >= 1 then (
    let pattern = tokens.(0) in
    let infix = insert_concat pattern in
    let postfix = to_postfix infix in

    let nfa = ref [||] in
    let add_state () =
      let id = Array.length !nfa in
      nfa := Array.append !nfa [| ref [] |];
      id
    in
    let add_trans u v c =
      !nfa.(u) := { ch = c; to_state = v } :: !(!nfa.(u))
    in

    let st = Stack.create () in
    for i = 0 to String.length postfix - 1 do
      let c = postfix.[i] in
      if c = '.' then (
        let f2 = Stack.pop st in
        let f1 = Stack.pop st in
        add_trans f1.s_end f2.s_start '\000';
        Stack.push { s_start = f1.s_start; s_end = f2.s_end } st
      ) else if c = '|' then (
        let f2 = Stack.pop st in
        let f1 = Stack.pop st in
        let s = add_state () in
        let e = add_state () in
        add_trans s f1.s_start '\000';
        add_trans s f2.s_start '\000';
        add_trans f1.s_end e '\000';
        add_trans f2.s_end e '\000';
        Stack.push { s_start = s; s_end = e } st
      ) else if c = '*' then (
        let f = Stack.pop st in
        let s = add_state () in
        let e = add_state () in
        add_trans s f.s_start '\000';
        add_trans s e '\000';
        add_trans f.s_end f.s_start '\000';
        add_trans f.s_end e '\000';
        Stack.push { s_start = s; s_end = e } st
      ) else (
        let s = add_state () in
        let e = add_state () in
        add_trans s e c;
        Stack.push { s_start = s; s_end = e } st
      )
    done;

    let frag = Stack.pop st in

    let rec get_closure s closure =
      if IntSet.mem s !closure then ()
      else (
        closure := IntSet.add s !closure;
        List.iter (fun tr ->
          if tr.ch = '\000' then get_closure tr.to_state closure
        ) !(!nfa.(s))
      )
    in

    let match_str s =
      let current = ref IntSet.empty in
      get_closure frag.s_start current;
      let ok = ref true in
      let len = String.length s in
      let i = ref 0 in
      while !ok && !i < len do
        let c = s.[!i] in
        let next_states = ref IntSet.empty in
        IntSet.iter (fun state ->
          List.iter (fun tr ->
            if tr.ch = c then get_closure tr.to_state next_states
          ) !(!nfa.(state))
        ) !current;
        current := !next_states;
        if IntSet.is_empty !current then ok := false;
        incr i
      done;
      !ok && IntSet.mem frag.s_end !current
    in

    if Array.length tokens >= 2 then (
      let k = int_of_string tokens.(1) in
      let idx = ref 2 in
      for _ = 1 to k do
        if !idx < Array.length tokens then (
          let test_str = tokens.(!idx) in
          incr idx;
          let test_str = if test_str = "EPS" then "" else test_str in
          if match_str test_str then
            Printf.printf "MATCH\n"
          else
            Printf.printf "NO_MATCH\n"
        )
      done
    )
  )
