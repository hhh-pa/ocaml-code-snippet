type node = {
  next_node : int array;
  mutable fail : int;
  mutable dict_link : int;
  mutable gene_ids : int list;
  mutable pref_health : int64 list;
  mutable gene_ids_arr : int array;
  mutable pref_health_arr : int64 array;
}

let make_node () = {
  next_node = Array.make 26 0;
  fail = 0;
  dict_link = 0;
  gene_ids = [];
  pref_health = [0L];
  gene_ids_arr = [||];
  pref_health_arr = [| 0L |];
}

let bisect_left (arr : int array) (x : int) =
  let lo = ref 0 in
  let hi = ref (Array.length arr) in
  while !lo < !hi do
    let mid = (!lo + !hi) / 2 in
    if arr.(mid) < x then lo := mid + 1
    else hi := mid
  done;
  !lo

let bisect_right (arr : int array) (x : int) =
  let lo = ref 0 in
  let hi = ref (Array.length arr) in
  while !lo < !hi do
    let mid = (!lo + !hi) / 2 in
    if arr.(mid) <= x then lo := mid + 1
    else hi := mid
  done;
  !lo

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let genes = Array.init n (fun _ -> Scanf.scanf " %s" (fun x -> x)) in
    let gene_health = Array.init n (fun _ -> Scanf.scanf " %Ld" (fun x -> x)) in

    let trie = ref [| make_node () |] in
    let new_node () =
      let node = make_node () in
      let idx = Array.length !trie in
      let new_arr = Array.make (idx + 1) node in
      Array.blit !trie 0 new_arr 0 idx;
      trie := new_arr;
      idx
    in

    for i = 0 to n - 1 do
      let s = genes.(i) in
      let u = ref 0 in
      for j = 0 to String.length s - 1 do
        let c = Char.code s.[j] - Char.code 'a' in
        if !trie.(!u).next_node.(c) = 0 then begin
          let next_idx = new_node () in
          !trie.(!u).next_node.(c) <- next_idx
        end;
        u := !trie.(!u).next_node.(c)
      done;
      let target = !trie.(!u) in
      target.gene_ids <- i :: target.gene_ids;
      let last_h = List.hd target.pref_health in
      target.pref_health <- (Int64.add last_h gene_health.(i)) :: target.pref_health
    done;

    for i = 0 to Array.length !trie - 1 do
      let node = !trie.(i) in
      node.gene_ids_arr <- Array.of_list (List.rev node.gene_ids);
      node.pref_health_arr <- Array.of_list (List.rev node.pref_health)
    done;

    let q = Queue.create () in
    for c = 0 to 25 do
      let v = !trie.(0).next_node.(c) in
      if v <> 0 then Queue.push v q
    done;

    while not (Queue.is_empty q) do
      let u = Queue.pop q in
      for c = 0 to 25 do
        let v = !trie.(u).next_node.(c) in
        if v <> 0 then begin
          let f = ref !trie.(u).fail in
          while !f <> 0 && !trie.(!f).next_node.(c) = 0 do
            f := !trie.(!f).fail
          done;
          !trie.(v).fail <- !trie.(!f).next_node.(c);
          let fail_node = !trie.(!trie.(v).fail) in
          !trie.(v).dict_link <- (
            if Array.length fail_node.gene_ids_arr > 0 then !trie.(v).fail
            else fail_node.dict_link
          );
          Queue.push v q
        end
      done
    done;

    let s_count = Scanf.scanf " %d" (fun x -> x) in
    let min_health = ref (-1L) in
    let max_health = ref (-1L) in

    for _ = 0 to s_count - 1 do
      let first = Scanf.scanf " %d" (fun x -> x) in
      let last = Scanf.scanf " %d" (fun x -> x) in
      let strand = Scanf.scanf " %s" (fun x -> x) in

      let total = ref 0L in
      let u = ref 0 in
      for i = 0 to String.length strand - 1 do
        let c = Char.code strand.[i] - Char.code 'a' in
        while !u <> 0 && !trie.(!u).next_node.(c) = 0 do
          u := !trie.(!u).fail
        done;
        u := !trie.(!u).next_node.(c);

        let curr = ref !u in
        while !curr <> 0 do
          let node = !trie.(!curr) in
          if Array.length node.gene_ids_arr > 0 then begin
            let l_idx = bisect_left node.gene_ids_arr first in
            let r_idx = bisect_right node.gene_ids_arr last in
            if r_idx > l_idx then begin
              let h = Int64.sub node.pref_health_arr.(r_idx) node.pref_health_arr.(l_idx) in
              total := Int64.add !total h
            end
          end;
          curr := node.dict_link
        done
      done;

      if !min_health = -1L || !total < !min_health then min_health := !total;
      if !max_health = -1L || !total > !max_health then max_health := !total
    done;

    Printf.printf "%Ld %Ld\n" !min_health !max_health
  with End_of_file -> ()
