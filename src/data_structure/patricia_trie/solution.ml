module CharMap = Map.Make(Char)

type radix_node = {
  mutable edge : string;
  mutable has_val : bool;
  mutable val_data : int64;
  mutable sub_count : int64;
  mutable children : radix_node CharMap.t;
}

let lcp_len a b =
  let n = min (String.length a) (String.length b) in
  let i = ref 0 in
  while !i < n && a.[!i] = b.[!i] do incr i done;
  !i

let update_count u =
  let cnt = ref (if u.has_val then 1L else 0L) in
  CharMap.iter (fun _ child ->
    cnt := Int64.add !cnt child.sub_count
  ) u.children;
  u.sub_count <- !cnt

let rec insert_rec u rem v_data =
  let rem_len = String.length rem in
  if rem_len = 0 then begin
    u.has_val <- true;
    u.val_data <- v_data;
    update_count u
  end else
    let c = rem.[0] in
    match CharMap.find_opt c u.children with
    | None ->
      let leaf = {
        edge = rem;
        has_val = true;
        val_data = v_data;
        sub_count = 1L;
        children = CharMap.empty;
      } in
      u.children <- CharMap.add c leaf u.children;
      update_count u
    | Some v ->
      let k = lcp_len rem v.edge in
      let v_edge_len = String.length v.edge in
      if k = v_edge_len then begin
        insert_rec v (String.sub rem k (rem_len - k)) v_data;
        update_count u
      end else begin
        let mid = {
          edge = String.sub v.edge 0 k;
          has_val = false;
          val_data = 0L;
          sub_count = 0L;
          children = CharMap.empty;
        } in
        v.edge <- String.sub v.edge k (v_edge_len - k);
        let old_c = v.edge.[0] in
        mid.children <- CharMap.add old_c v mid.children;
        if k = rem_len then begin
          mid.has_val <- true;
          mid.val_data <- v_data
        end else begin
          let leaf_edge = String.sub rem k (rem_len - k) in
          let leaf = {
            edge = leaf_edge;
            has_val = true;
            val_data = v_data;
            sub_count = 1L;
            children = CharMap.empty;
          } in
          mid.children <- CharMap.add leaf_edge.[0] leaf mid.children
        end;
        update_count mid;
        u.children <- CharMap.add c mid u.children;
        update_count u
      end

let rec erase_rec u rem =
  let rem_len = String.length rem in
  if rem_len = 0 then begin
    if u.has_val then begin
      u.has_val <- false;
      update_count u
    end
  end else
    let c = rem.[0] in
    match CharMap.find_opt c u.children with
    | None -> ()
    | Some v ->
      let k = lcp_len rem v.edge in
      if k = String.length v.edge then begin
        erase_rec v (String.sub rem k (rem_len - k));
        let num_children = CharMap.cardinal v.children in
        if not v.has_val && num_children = 0 then
          u.children <- CharMap.remove c u.children
        else if not v.has_val && num_children = 1 then begin
          let (_, grandchild) = CharMap.choose v.children in
          grandchild.edge <- v.edge ^ grandchild.edge;
          u.children <- CharMap.add c grandchild u.children
        end;
        update_count u
      end

let search root word =
  let rec loop u rem =
    let rem_len = String.length rem in
    if rem_len = 0 then
      if u.has_val then Some u.val_data else None
    else
      let c = rem.[0] in
      match CharMap.find_opt c u.children with
      | None -> None
      | Some v ->
        let k = lcp_len rem v.edge in
        if k < String.length v.edge then None
        else loop v (String.sub rem k (rem_len - k))
  in
  loop root word

let count_prefix root prefix =
  let rec loop u rem =
    let rem_len = String.length rem in
    if rem_len = 0 then u.sub_count
    else
      let c = rem.[0] in
      match CharMap.find_opt c u.children with
      | None -> 0L
      | Some v ->
        let k = lcp_len rem v.edge in
        if k = rem_len then v.sub_count
        else if k < String.length v.edge then 0L
        else loop v (String.sub rem k (rem_len - k))
  in
  loop root prefix

let () =
  let tokens =
    let buf = Buffer.create 4096 in
    (try while true do Buffer.add_channel buf stdin 4096 done with End_of_file -> ());
    let s = Buffer.contents buf in
    let len = String.length s in
    let res = ref [] in
    let i = ref 0 in
    while !i < len do
      while !i < len && (let c = s.[!i] in c = ' ' || c = '\t' || c = '\r' || c = '\n') do incr i done;
      if !i < len then begin
        let st = !i in
        while !i < len && (let c = s.[!i] in c <> ' ' && c <> '\t' && c <> '\r' && c <> '\n') do incr i done;
        res := String.sub s st (!i - st) :: !res
      end
    done;
    Array.of_list (List.rev !res)
  in
  if Array.length tokens >= 1 then begin
    let pos = ref 0 in
    let next_str () = let t = tokens.(!pos) in incr pos; t in
    let next_int () = int_of_string (next_str ()) in
    let next_int64 () = Int64.of_string (next_str ()) in
    let q = next_int () in
    let root = {
      edge = "";
      has_val = false;
      val_data = 0L;
      sub_count = 0L;
      children = CharMap.empty;
    } in
    for _ = 0 to q - 1 do
      if !pos < Array.length tokens then begin
        let op = next_int () in
        let word = next_str () in
        if op = 1 then begin
          let v = next_int64 () in
          insert_rec root word v
        end else if op = 2 then begin
          erase_rec root word
        end else if op = 3 then begin
          match search root word with
          | Some v -> Printf.printf "%Ld\n" v
          | None -> print_endline "NOT_FOUND"
        end else if op = 4 then begin
          Printf.printf "%Ld\n" (count_prefix root word)
        end
      end
    done
  end
