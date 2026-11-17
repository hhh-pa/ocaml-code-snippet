(* String function calculation (HackerRank):
   https://www.hackerrank.com/challenges/string-function-calculation/problem
   Max over distinct substrings of length times occurrences, via Ukkonen's
   online suffix-tree construction plus string-depth times leaf-count max. *)

type snode = {
  mutable link : int;
  mutable start : int;
  mutable end_ : int;
  leaf : bool;
  next : int array;
}

type state = {
  nodes : (int, snode) Hashtbl.t;
  mutable next_id : int;
  text : string;
  mutable active : int;
  mutable edge : int;
  mutable alen : int;
  mutable rem : int;
  mutable last_new : int;
  mutable pos : int;
}

let suf_none = -1
let suf_sym c = if c = '$' then 26 else Char.code c - Char.code 'a'

let suf_new_node st start end_ leaf =
  let id = st.next_id in
  st.next_id <- id + 1;
  Hashtbl.add st.nodes id
    { link = 0; start; end_; leaf; next = Array.make 27 suf_none };
  id

let suf_finish st =
  st.rem <- st.rem - 1;
  if st.active = 0 && st.alen > 0 then begin
    st.alen <- st.alen - 1;
    st.edge <- st.pos - st.rem + 1
  end
  else if st.active <> 0 then
    st.active <- (Hashtbl.find st.nodes st.active).link

let suf_add_leaf st cur =
  let leaf = suf_new_node st st.pos 0 true in
  (Hashtbl.find st.nodes st.active).next.(cur) <- leaf;
  if st.last_new <> suf_none then
    (Hashtbl.find st.nodes st.last_new).link <- st.active;
  st.last_new <- suf_none

let suf_split st cur nxt epos =
  let nd_start = (Hashtbl.find st.nodes nxt).start in
  let split = suf_new_node st nd_start (nd_start + st.alen - 1) false in
  let leaf = suf_new_node st st.pos 0 true in
  (Hashtbl.find st.nodes st.active).next.(cur) <- split;
  (Hashtbl.find st.nodes split).next.(suf_sym st.text.[st.pos]) <- leaf;
  let nd = Hashtbl.find st.nodes nxt in
  nd.start <- nd.start + st.alen;
  (Hashtbl.find st.nodes split).next.(suf_sym st.text.[epos]) <- nxt;
  if st.last_new <> suf_none then
    (Hashtbl.find st.nodes st.last_new).link <- split;
  st.last_new <- split

let suf_step st =
  if st.alen = 0 then st.edge <- st.pos;
  let cur = suf_sym st.text.[st.edge] in
  let nxt = (Hashtbl.find st.nodes st.active).next.(cur) in
  if nxt = suf_none then begin
    suf_add_leaf st cur;
    suf_finish st;
    true
  end
  else
    let nd = Hashtbl.find st.nodes nxt in
    let elen = (if nd.leaf then st.pos else nd.end_) - nd.start + 1 in
    if st.alen >= elen then begin
      st.edge <- st.edge + elen;
      st.alen <- st.alen - elen;
      st.active <- nxt;
      true
    end
    else
      let epos = nd.start + st.alen in
      if st.text.[epos] = st.text.[st.pos] then begin
        if st.last_new <> suf_none && st.active <> 0 then
          (Hashtbl.find st.nodes st.last_new).link <- st.active;
        st.alen <- st.alen + 1;
        false
      end
      else begin
        suf_split st cur nxt epos;
        suf_finish st;
        true
      end

let suf_build text =
  let st =
    {
      nodes = Hashtbl.create 16;
      next_id = 0;
      text;
      active = 0;
      edge = 0;
      alen = 0;
      rem = 0;
      last_new = suf_none;
      pos = 0;
    }
  in
  ignore (suf_new_node st 0 (-1) false);
  for p = 0 to String.length text - 1 do
    st.pos <- p;
    st.rem <- st.rem + 1;
    st.last_new <- suf_none;
    while st.rem > 0 && suf_step st do
      ()
    done
  done;
  (st.nodes, st.next_id)

let suf_value nodes nnodes n =
  let order = ref [] in
  let stack = ref [ 0 ] in
  while !stack <> [] do
    let u = List.hd !stack in
    stack := List.tl !stack;
    order := u :: !order;
    let nd = Hashtbl.find nodes u in
    for w = 0 to 26 do
      let v = nd.next.(w) in
      if v <> suf_none then stack := v :: !stack
    done
  done;
  let order = List.rev !order in
  let depth = Array.make nnodes 0 in
  List.iter
    (fun v ->
      let dv = depth.(v) in
      let nd = Hashtbl.find nodes v in
      for w = 0 to 26 do
        let u = nd.next.(w) in
        if u <> suf_none then
          let nd2 = Hashtbl.find nodes u in
          depth.(u) <-
            (dv + if nd2.leaf then n - nd2.start else nd2.end_ - nd2.start + 1)
      done)
    order;
  let count = Array.make nnodes 0 in
  let ans = ref n in
  List.iter
    (fun v ->
      let nd = Hashtbl.find nodes v in
      let c =
        if nd.leaf then 1
        else begin
          let s = ref 0 in
          for w = 0 to 26 do
            let u = nd.next.(w) in
            if u <> suf_none then s := !s + count.(u)
          done;
          !s
        end
      in
      count.(v) <- c;
      ans := max !ans (depth.(v) * c))
    (List.rev order);
  !ans

let read_all () =
  let buf = Buffer.create 256 in
  (try
     while true do
       Buffer.add_char buf (input_char stdin)
     done
   with End_of_file -> ());
  Buffer.contents buf

let is_space = function ' ' | '\t' | '\n' | '\r' -> true | _ -> false

let split_ws s =
  let toks = ref [] in
  let cur = Buffer.create 16 in
  let flush () =
    if Buffer.length cur > 0 then begin
      toks := Buffer.contents cur :: !toks;
      Buffer.clear cur
    end
  in
  String.iter
    (fun c -> if is_space c then flush () else Buffer.add_char cur c)
    s;
  flush ();
  List.rev !toks

let () =
  match split_ws (read_all ()) with
  | [] -> ()
  | s :: _ ->
      let ok = ref true in
      String.iter (fun c -> if c < 'a' || c > 'z' then ok := false) s;
      if not !ok then
        Printf.eprintf "error: expected a single lowercase string token\n"
      else
        let n = String.length s in
        let nodes, nnodes = suf_build (s ^ "$") in
        Printf.printf "%d\n" (suf_value nodes nnodes n)
