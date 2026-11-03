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

type state = {
  mutable len : int;
  mutable link : int;
  next : int array;
}

let create_state length lk = {
  len = length;
  link = lk;
  next = Array.make 128 (-1);
}

let clone_state st_q new_len = {
  len = new_len;
  link = st_q.link;
  next = Array.copy st_q.next;
}

let () =
  let tokens = read_all_tokens () in
  if Array.length tokens >= 1 then (
    let s = tokens.(0) in
    let states = ref (Array.make 16 (create_state 0 (-1))) in
    !states.(0) <- create_state 0 (-1);
    let size = ref 1 in
    let last = ref 0 in

    let add_state st =
      if !size = Array.length !states then (
        let old = !states in
        let nw = Array.make (Array.length old * 2) (create_state 0 (-1)) in
        Array.blit old 0 nw 0 !size;
        states := nw
      );
      let id = !size in
      !states.(id) <- st;
      incr size;
      id
    in

    let extend ch =
      let c = Char.code ch in
      let cur = add_state (create_state (!states.(!last).len + 1) (-1)) in
      let p = ref !last in
      while !p <> -1 && !states.(!p).next.(c) = -1 do
        !states.(!p).next.(c) <- cur;
        p := !states.(!p).link
      done;
      if !p = -1 then
        !states.(cur).link <- 0
      else (
        let q = !states.(!p).next.(c) in
        if !states.(!p).len + 1 = !states.(q).len then
          !states.(cur).link <- q
        else (
          let clone = add_state (clone_state !states.(q) (!states.(!p).len + 1)) in
          while !p <> -1 && !states.(!p).next.(c) = q do
            !states.(!p).next.(c) <- clone;
            p := !states.(!p).link
          done;
          !states.(q).link <- clone;
          !states.(cur).link <- clone
        )
      );
      last := cur
    in

    let slen = String.length s in
    for i = 0 to slen - 1 do
      extend s.[i]
    done;

    let tr_count = ref 0 in
    for i = 0 to !size - 1 do
      for c = 0 to 127 do
        if !states.(i).next.(c) <> -1 then incr tr_count
      done
    done;

    let distinct_sub = ref 0L in
    for i = 1 to !size - 1 do
      let link_len = if !states.(i).link = -1 then 0 else !states.(!states.(i).link).len in
      distinct_sub := Int64.add !distinct_sub (Int64.of_int (!states.(i).len - link_len))
    done;

    let contains pat =
      let cur = ref 0 in
      let ok = ref true in
      let plen = String.length pat in
      let i = ref 0 in
      while !i < plen && !ok do
        let c = Char.code pat.[!i] in
        let nxt = !states.(!cur).next.(c) in
        if nxt = -1 then ok := false
        else cur := nxt;
        incr i
      done;
      !ok
    in

    Printf.printf "States: %d\n" !size;
    Printf.printf "Transitions: %d\n" !tr_count;
    Printf.printf "Distinct substrings: %Ld\n" !distinct_sub;

    if Array.length tokens >= 2 then (
      let num_queries = int_of_string tokens.(1) in
      for q = 0 to num_queries - 1 do
        let pat = tokens.(2 + q) in
        if contains pat then print_endline "YES"
        else print_endline "NO"
      done
    )
  )
