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

type suffix_node = {
  children : int array;
  mutable suffix_link : int;
  mutable start : int;
  mutable end_val : int;
  is_leaf : bool;
}

let create_node start end_val is_leaf = {
  children = Array.make 128 (-1);
  suffix_link = 0;
  start;
  end_val;
  is_leaf;
}

let () =
  let tokens = read_all_tokens () in
  if Array.length tokens >= 1 then (
    let text = tokens.(0) in
    let tree = ref (Array.make 16 (create_node 0 (-1) false)) in
    !tree.(0) <- create_node 0 (-1) false;
    let tree_size = ref 1 in
    let root = 0 in

    let new_node start end_val is_leaf =
      if !tree_size = Array.length !tree then (
        let old = !tree in
        let nw = Array.make (Array.length old * 2) (create_node 0 (-1) false) in
        Array.blit old 0 nw 0 !tree_size;
        tree := nw
      );
      let id = !tree_size in
      !tree.(id) <- create_node start end_val is_leaf;
      !tree.(id).suffix_link <- root;
      incr tree_size;
      id
    in

    let leaf_end = ref (-1) in

    let get_end n_idx =
      let node = !tree.(n_idx) in
      if node.is_leaf then !leaf_end
      else node.end_val
    in

    let edge_length n_idx =
      if n_idx = root then 0
      else get_end n_idx - !tree.(n_idx).start + 1
    in

    let active_node = ref 0 in
    let active_edge = ref (-1) in
    let active_length = ref 0 in
    let remaining_suffix_count = ref 0 in

    let walk_down curr_node =
      let elen = edge_length curr_node in
      if !active_length >= elen then (
        active_edge := !active_edge + elen;
        active_length := !active_length - elen;
        active_node := curr_node;
        true
      ) else false
    in

    let extend pos =
      leaf_end := pos;
      incr remaining_suffix_count;
      let last_new_node = ref (-1) in
      let break_outer = ref false in

      while !remaining_suffix_count > 0 && not !break_outer do
        if !active_length = 0 then
          active_edge := pos;
        let cur_char = Char.code text.[!active_edge] in
        let next_node = !tree.(!active_node).children.(cur_char) in

        if next_node = -1 then (
          let leaf = new_node pos (-1) true in
          !tree.(!active_node).children.(cur_char) <- leaf;
          if !last_new_node <> -1 then (
            !tree.(!last_new_node).suffix_link <- !active_node;
            last_new_node := -1
          );
          decr remaining_suffix_count;
          if !active_node = root && !active_length > 0 then (
            decr active_length;
            active_edge := pos - !remaining_suffix_count + 1
          ) else if !active_node <> root then (
            active_node := !tree.(!active_node).suffix_link
          )
        ) else if walk_down next_node then (
          ()
        ) else (
          let edge_char_pos = !tree.(next_node).start + !active_length in
          if text.[edge_char_pos] = text.[pos] then (
            if !last_new_node <> -1 && !active_node <> root then (
              !tree.(!last_new_node).suffix_link <- !active_node;
              last_new_node := -1
            );
            incr active_length;
            break_outer := true
          ) else (
            let split_end = !tree.(next_node).start + !active_length - 1 in
            let split = new_node !tree.(next_node).start split_end false in
            !tree.(!active_node).children.(cur_char) <- split;

            let leaf = new_node pos (-1) true in
            !tree.(split).children.(Char.code text.[pos]) <- leaf;

            !tree.(next_node).start <- !tree.(next_node).start + !active_length;
            !tree.(split).children.(Char.code text.[edge_char_pos]) <- next_node;

            if !last_new_node <> -1 then
              !tree.(!last_new_node).suffix_link <- split;
            last_new_node := split;

            decr remaining_suffix_count;
            if !active_node = root && !active_length > 0 then (
              decr active_length;
              active_edge := pos - !remaining_suffix_count + 1
            ) else if !active_node <> root then (
              active_node := !tree.(!active_node).suffix_link
            )
          )
        )
      done
    in

    let tlen = String.length text in
    for i = 0 to tlen - 1 do
      extend i
    done;

    let rec count_distinct_substrings u =
      let count = ref 0L in
      for c = 0 to 127 do
        let v = !tree.(u).children.(c) in
        if v <> -1 then (
          let elen = Int64.of_int (get_end v - !tree.(v).start + 1) in
          let sub_count = count_distinct_substrings v in
          count := Int64.add !count (Int64.add elen sub_count)
        )
      done;
      !count
    in

    let find_pattern pat =
      let cur = ref root in
      let idx = ref 0 in
      let plen = String.length pat in
      let ok = ref true in
      while !idx < plen && !ok do
        let c = Char.code pat.[!idx] in
        let nxt = !tree.(!cur).children.(c) in
        if nxt = -1 then ok := false
        else (
          let st = !tree.(nxt).start in
          let ed = get_end nxt in
          let edge_len = ed - st + 1 in
          let k = ref 0 in
          while !k < edge_len && !idx < plen && !ok do
            if text.[st + !k] <> pat.[!idx] then ok := false;
            incr k;
            incr idx
          done;
          cur := nxt
        )
      done;
      !ok
    in

    Printf.printf "Nodes: %d\n" !tree_size;
    Printf.printf "Distinct substrings: %Ld\n" (count_distinct_substrings 0);

    if Array.length tokens >= 2 then (
      let num_queries = int_of_string tokens.(1) in
      for q = 0 to num_queries - 1 do
        let pat = tokens.(2 + q) in
        if find_pattern pat then print_endline "YES"
        else print_endline "NO"
      done
    )
  )
