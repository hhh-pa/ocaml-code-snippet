type node = {
  key : int64;
  prio : int;
  mutable sz : int;
  mutable left : node option;
  mutable right : node option;
}

let get_sz = function
  | None -> 0
  | Some n -> n.sz

let update_sz n =
  n.sz <- 1 + get_sz n.left + get_sz n.right

let rec split t key =
  match t with
  | None -> (None, None)
  | Some n ->
    if key < n.key then
      let l, r = split n.left key in
      n.left <- r;
      update_sz n;
      (l, Some n)
    else
      let l, r = split n.right key in
      n.right <- l;
      update_sz n;
      (Some n, r)

let rec merge l r =
  match l, r with
  | None, _ -> r
  | _, None -> l
  | Some nl, Some nr ->
    if nl.prio > nr.prio then begin
      nl.right <- merge nl.right r;
      update_sz nl;
      Some nl
    end else begin
      nr.left <- merge l nr.left;
      update_sz nr;
      Some nr
    end

let seed = ref 1337
let rng () =
  seed := (!seed * 1103515245 + 12345) land 0x7fffffff;
  !seed

let insert root_ref key =
  let l, r = split !root_ref key in
  let rec get_max = function
    | None -> None
    | Some n -> (match n.right with None -> Some n.key | Some _ -> get_max n.right)
  in
  if get_max l = Some key then
    root_ref := merge l r
  else
    let m = Some { key; prio = rng (); sz = 1; left = None; right = None } in
    root_ref := merge (merge l m) r

let remove root_ref key =
  let l, r = split !root_ref (Int64.sub key 1L) in
  let _, r2 = split r key in
  root_ref := merge l r2

let os_select root_ref k =
  if k < 1 || k > get_sz !root_ref then None
  else
    let rec find cur k_rem =
      match cur with
      | None -> None
      | Some n ->
        let left_sz = get_sz n.left in
        if k_rem = left_sz + 1 then Some n.key
        else if k_rem <= left_sz then find n.left k_rem
        else find n.right (k_rem - left_sz - 1)
    in
    find !root_ref k

let os_rank root_ref key =
  let rec count cur =
    match cur with
    | None -> 0
    | Some n ->
      if key <= n.key then count n.left
      else get_sz n.left + 1 + count n.right
  in
  count !root_ref

let fenwick_josephus n m =
  let bit = Array.make (n + 1) 0 in
  let add i d =
    let idx = ref i in
    while !idx <= n do
      bit.(!idx) <- bit.(!idx) + d;
      idx := !idx + (!idx land (- !idx))
    done
  in
  for i = 1 to n do
    add i 1
  done;
  let kth k_target =
    let k = ref k_target in
    let i = ref 0 in
    let step = ref 1 in
    while !step * 2 <= n do
      step := !step * 2
    done;
    while !step > 0 do
      let j = !i + !step in
      if j <= n && bit.(j) < !k then begin
        k := !k - bit.(j);
        i := j
      end;
      step := !step / 2
    done;
    !i + 1
  in
  let pos = ref 0 in
  let rem = ref n in
  let order = ref [] in
  while !rem > 0 do
    pos := (!pos + m - 1) mod !rem;
    let idx = kth (!pos + 1) in
    order := idx :: !order;
    add idx (-1);
    decr rem
  done;
  List.rev !order

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
    let root = ref None in
    for _ = 0 to q - 1 do
      if !pos < Array.length tokens then begin
        let op = next_int () in
        if op = 1 then begin
          let x = next_int64 () in
          insert root x
        end else if op = 2 then begin
          let x = next_int64 () in
          remove root x
        end else if op = 3 then begin
          let k = next_int () in
          match os_select root k with
          | Some v -> Printf.printf "%Ld\n" v
          | None -> print_endline "NONE"
        end else if op = 4 then begin
          let x = next_int64 () in
          Printf.printf "%d\n" (os_rank root x)
        end else if op = 5 then begin
          let n = next_int () in
          let m = next_int () in
          let res = fenwick_josephus n m in
          let strs = List.map string_of_int res in
          print_endline (String.concat " " strs)
        end
      end
    done
  end
