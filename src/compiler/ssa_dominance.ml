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
  if Array.length tokens >= 2 then (
    let n = int_of_string tokens.(0) in
    let m = int_of_string tokens.(1) in
    let idx = ref 2 in

    let adj = Array.make (n + 1) [] in
    let rev_adj = Array.make (n + 1) [] in
    for _ = 1 to m do
      if !idx + 1 < Array.length tokens then (
        let u = int_of_string tokens.(!idx) in
        let v = int_of_string tokens.(!idx + 1) in
        idx := !idx + 2;
        adj.(u) <- v :: adj.(u);
        rev_adj.(v) <- u :: rev_adj.(v)
      )
    done;
    for i = 1 to n do
      adj.(i) <- List.rev adj.(i);
      rev_adj.(i) <- List.rev rev_adj.(i)
    done;

    let dom = Array.make (n + 1) IntSet.empty in
    dom.(1) <- IntSet.singleton 1;
    let all_nodes = ref IntSet.empty in
    for i = 1 to n do
      all_nodes := IntSet.add i !all_nodes
    done;
    for i = 2 to n do
      dom.(i) <- !all_nodes
    done;

    let changed = ref true in
    while !changed do
      changed := false;
      for i = 2 to n do
        let new_dom =
          match rev_adj.(i) with
          | [] -> IntSet.empty
          | p :: rest ->
            List.fold_left (fun acc q -> IntSet.inter acc dom.(q)) dom.(p) rest
        in
        let new_dom = IntSet.add i new_dom in
        if not (IntSet.equal new_dom dom.(i)) then (
          dom.(i) <- new_dom;
          changed := true
        )
      done
    done;

    let idom = Array.make (n + 1) 0 in
    for i = 2 to n do
      let sz_i = IntSet.cardinal dom.(i) in
      IntSet.iter (fun d ->
        if d <> i && sz_i = IntSet.cardinal dom.(d) + 1 then
          idom.(i) <- d
      ) dom.(i)
    done;

    let df = Array.make (n + 1) IntSet.empty in
    for x = 1 to n do
      List.iter (fun y ->
        if idom.(y) <> x then
          df.(x) <- IntSet.add y df.(x)
      ) adj.(x)
    done;

    for x = 1 to n do
      for z = 1 to n do
        if idom.(z) = x then (
          IntSet.iter (fun y ->
            if idom.(y) <> x then
              df.(x) <- IntSet.add y df.(x)
          ) df.(z)
        )
      done
    done;

    if !idx < Array.length tokens then (
      let def_count = int_of_string tokens.(!idx) in
      incr idx;
      let defs = ref IntSet.empty in
      for _ = 1 to def_count do
        if !idx < Array.length tokens then (
          defs := IntSet.add (int_of_string tokens.(!idx)) !defs;
          incr idx
        )
      done;

      let phi_blocks = ref IntSet.empty in
      let w = Queue.create () in
      IntSet.iter (fun d -> Queue.push d w) !defs;

      while not (Queue.is_empty w) do
        let x = Queue.pop w in
        IntSet.iter (fun y ->
          if not (IntSet.mem y !phi_blocks) then (
            phi_blocks := IntSet.add y !phi_blocks;
            if not (IntSet.mem y !defs) then Queue.push y w
          )
        ) df.(x)
      done;

      for i = 2 to n do
        Printf.printf "idom(%d)=%d " i idom.(i)
      done;
      print_newline ();

      if IntSet.is_empty !phi_blocks then
        Printf.printf "NONE\n"
      else (
        let blist = IntSet.elements !phi_blocks in
        let bstrs = List.map string_of_int blist in
        Printf.printf "%s\n" (String.concat " " bstrs)
      )
    )
  )
