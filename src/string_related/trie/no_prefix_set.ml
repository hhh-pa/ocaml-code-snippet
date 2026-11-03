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

type trie_node = {
  children : int array;
  mutable end_node : bool;
}

let create_node () = {
  children = Array.make 26 (-1);
  end_node = false;
}

let () =
  let tokens = read_all_tokens () in
  if Array.length tokens >= 1 then (
    let n = int_of_string tokens.(0) in
    let nodes = ref (Array.make 16 (create_node ())) in
    !nodes.(0) <- create_node ();
    let size = ref 1 in

    let add_node () =
      if !size = Array.length !nodes then (
        let old = !nodes in
        let nw = Array.make (Array.length old * 2) (create_node ()) in
        Array.blit old 0 nw 0 !size;
        nodes := nw
      );
      let id = !size in
      !nodes.(id) <- create_node ();
      incr size;
      id
    in

    let add_word w =
      let curr = ref 0 in
      let is_bad = ref false in
      let len = String.length w in
      for i = 0 to len - 1 do
        let c = Char.code w.[i] - Char.code 'a' in
        if !nodes.(!curr).children.(c) = -1 then (
          let nxt = add_node () in
          !nodes.(!curr).children.(c) <- nxt
        );
        curr := !nodes.(!curr).children.(c);
        if !nodes.(!curr).end_node then
          is_bad := true
      done;
      let has_children = ref false in
      for c = 0 to 25 do
        if !nodes.(!curr).children.(c) <> -1 then has_children := true
      done;
      if !has_children then is_bad := true;
      !nodes.(!curr).end_node <- true;
      !is_bad
    in

    let found_bad = ref false in
    let i = ref 1 in
    while !i <= n && not !found_bad do
      let s = tokens.(!i) in
      if add_word s then (
        print_endline "BAD SET";
        print_endline s;
        found_bad := true
      );
      incr i
    done;
    if not !found_bad then
      print_endline "GOOD SET"
  )
