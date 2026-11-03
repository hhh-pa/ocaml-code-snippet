type fib_node = {
  id: int;
  mutable key: int64;
  mutable degree: int;
  mutable marked: bool;
  mutable parent: fib_node option;
  mutable child: fib_node option;
  mutable left: fib_node option;
  mutable right: fib_node option;
}

let unwrap = function
  | Some x -> x
  | None -> failwith "unwrap"

let make_node id key =
  let n = {
    id;
    key;
    degree = 0;
    marked = false;
    parent = None;
    child = None;
    left = None;
    right = None;
  } in
  n.left <- Some n;
  n.right <- Some n;
  n

let link_roots y x =
  let y_left = unwrap y.left in
  let y_right = unwrap y.right in
  y_left.right <- Some y_right;
  y_right.left <- Some y_left;
  y.parent <- Some x;
  (match x.child with
   | None ->
       x.child <- Some y;
       y.left <- Some y;
       y.right <- Some y
   | Some c ->
       let c_left = unwrap c.left in
       y.right <- Some c;
       y.left <- Some c_left;
       c_left.right <- Some y;
       c.left <- Some y);
  x.degree <- x.degree + 1;
  y.marked <- false

let cut x y min_node =
  let x_right = unwrap x.right in
  if x_right == x then
    y.child <- None
  else begin
    let x_left = unwrap x.left in
    x_left.right <- Some x_right;
    x_right.left <- Some x_left;
    match y.child with
    | Some c when c == x -> y.child <- Some x_right
    | _ -> ()
  end;
  y.degree <- y.degree - 1;
  let mn = unwrap !min_node in
  let mn_left = unwrap mn.left in
  x.right <- Some mn;
  x.left <- Some mn_left;
  mn_left.right <- Some x;
  mn.left <- Some x;
  x.parent <- None;
  x.marked <- false

let rec cascading_cut y min_node =
  match y.parent with
  | None -> ()
  | Some z ->
      if not y.marked then
        y.marked <- true
      else begin
        cut y z min_node;
        cascading_cut z min_node
      end

let consolidate min_node =
  let max_deg = 50 in
  let a = Array.make max_deg None in
  let root_list = ref [] in
  (match !min_node with
   | None -> ()
   | Some mn ->
       let cur = ref mn in
       let cont = ref true in
       while !cont do
         root_list := !cur :: !root_list;
         cur := unwrap !cur.right;
         if !cur == mn then cont := false
       done);
  let roots = List.rev !root_list in
  List.iter (fun w ->
    let x = ref w in
    let d = ref !x.degree in
    while !d < max_deg && a.(!d) <> None do
      let y = ref (unwrap a.(!d)) in
      if !x.key > !y.key then begin
        let t = !x in x := !y; y := t
      end;
      link_roots !y !x;
      a.(!d) <- None;
      incr d
    done;
    if !d < max_deg then a.(!d) <- Some !x
  ) roots;

  min_node := None;
  for i = 0 to max_deg - 1 do
    match a.(i) with
    | None -> ()
    | Some ai ->
        match !min_node with
        | None ->
            min_node := Some ai;
            ai.left <- Some ai;
            ai.right <- Some ai
        | Some mn ->
            let mn_left = unwrap mn.left in
            ai.right <- Some mn;
            ai.left <- Some mn_left;
            mn_left.right <- Some ai;
            mn.left <- Some ai;
            if ai.key < mn.key then min_node := Some ai
  done

let insert min_node node_map id key =
  let node = make_node id key in
  Hashtbl.replace node_map id node;
  match !min_node with
  | None -> min_node := Some node
  | Some mn ->
      let mn_left = unwrap mn.left in
      node.right <- Some mn;
      node.left <- Some mn_left;
      mn_left.right <- Some node;
      mn.left <- Some node;
      if node.key < mn.key then min_node := Some node

let extract_min min_node node_map =
  match !min_node with
  | None -> None
  | Some z ->
      let out_id = z.id in
      let out_key = z.key in
      (match z.child with
       | None -> ()
       | Some ch ->
           let children = ref [] in
           let cur = ref ch in
           let cont = ref true in
           while !cont do
             children := !cur :: !children;
             cur := unwrap !cur.right;
             if !cur == ch then cont := false
           done;
           List.iter (fun c ->
             let mn = unwrap !min_node in
             let mn_left = unwrap mn.left in
             c.right <- Some mn;
             c.left <- Some mn_left;
             mn_left.right <- Some c;
             mn.left <- Some c;
             c.parent <- None
           ) (List.rev !children));

      let z_left = unwrap z.left in
      let z_right = unwrap z.right in
      z_left.right <- Some z_right;
      z_right.left <- Some z_left;

      if z_right == z then
        min_node := None
      else begin
        min_node := Some z_right;
        consolidate min_node
      end;
      Hashtbl.remove node_map z.id;
      Some (out_id, out_key)

let decrease_key min_node node_map id new_key =
  match Hashtbl.find_opt node_map id with
  | None -> ()
  | Some x ->
      if new_key <= x.key then begin
        x.key <- new_key;
        (match x.parent with
         | Some y ->
             if x.key < y.key then begin
               cut x y min_node;
               cascading_cut y min_node
             end
         | None -> ());
        match !min_node with
        | Some mn when x.key < mn.key -> min_node := Some x
        | _ -> ()
      end

let () =
  try
    let q = Scanf.scanf " %d" (fun x -> x) in
    let min_node = ref None in
    let node_map = Hashtbl.create 16 in
    for _ = 1 to q do
      let op = Scanf.scanf " %s" (fun x -> x) in
      if op = "INSERT" then
        let id = Scanf.scanf " %d" (fun x -> x) in
        let v = Scanf.scanf " %Ld" (fun x -> x) in
        insert min_node node_map id v
      else if op = "MIN" then
        match !min_node with
        | Some mn -> Printf.printf "%d %Ld\n" mn.id mn.key
        | None -> Printf.printf "EMPTY\n"
      else if op = "EXTRACT_MIN" then
        match extract_min min_node node_map with
        | Some (id, v) -> Printf.printf "%d %Ld\n" id v
        | None -> Printf.printf "EMPTY\n"
      else if op = "DECREASE_KEY" then
        let id = Scanf.scanf " %d" (fun x -> x) in
        let v = Scanf.scanf " %Ld" (fun x -> x) in
        decrease_key min_node node_map id v
    done
  with End_of_file -> ()
