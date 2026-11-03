type treap_node = {
  key : int;
  prio : int;
  mutable sz : int;
  mutable left : treap_node option;
  mutable right : treap_node option;
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
      let (l, r) = split n.left key in
      n.left <- r;
      update_sz n;
      (l, Some n)
    else
      let (l, r) = split n.right key in
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
    let q = next_int () in
    let root = ref None in

    let insert key =
      let (l, r) = split !root key in
      let rec get_max = function
        | None -> None
        | Some n -> (match n.right with None -> Some n.key | Some _ -> get_max n.right)
      in
      if get_max l = Some key then
        root := merge l r
      else
        let m = Some { key; prio = rng (); sz = 1; left = None; right = None } in
        root := merge (merge l m) r
    in

    let find key =
      let rec search = function
        | None -> false
        | Some n ->
          if n.key = key then true
          else if key < n.key then search n.left
          else search n.right
      in
      search !root
    in

    let erase key =
      let (l, r) = split !root (key - 1) in
      let (_, r2) = split r key in
      root := merge l r2
    in

    let get_inorder () =
      let res = ref [] in
      let rec walk = function
        | None -> ()
        | Some n ->
          walk n.left;
          res := n.key :: !res;
          walk n.right
      in
      walk !root;
      List.rev !res
    in

    for _ = 0 to q - 1 do
      if !pos < Array.length tokens then begin
        let op = next_str () in
        if op = "INSERT" then begin
          let v = next_int () in
          insert v
        end else if op = "FIND" then begin
          let v = next_int () in
          if find v then print_endline "FOUND"
          else print_endline "NOT_FOUND"
        end else if op = "ERASE" then begin
          let v = next_int () in
          erase v
        end else if op = "PRINT" then begin
          let items = get_inorder () in
          let strs = List.map string_of_int items in
          print_endline (String.concat " " strs)
        end
      end
    done
  end
