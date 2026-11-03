type node =
  | Nil
  | Node of int * node * node * int

let height = function
  | Nil -> -1
  | Node (_, _, _, h) -> h

let make_node v l r =
  let h = 1 + max (height l) (height r) in
  Node (v, l, r, h)

let balance_factor = function
  | Nil -> 0
  | Node (_, l, r, _) -> height l - height r

let rotate_right = function
  | Node (y, Node (x, t1, t2, _), t3, _) ->
      let new_y = make_node y t2 t3 in
      make_node x t1 new_y
  | n -> n

let rotate_left = function
  | Node (x, t1, Node (y, t2, t3, _), _) ->
      let new_x = make_node x t1 t2 in
      make_node y new_x t3
  | n -> n

let rec insert node val_ =
  match node with
  | Nil -> Node (val_, Nil, Nil, 0)
  | Node (v, l, r, _) ->
      if val_ < v then
        let l' = insert l val_ in
        let cur = make_node v l' r in
        let bf = balance_factor cur in
        if bf > 1 then
          match l' with
          | Node (lv, _, _, _) ->
              let l'' = if val_ > lv then rotate_left l' else l' in
              rotate_right (make_node v l'' r)
          | Nil -> cur
        else cur
      else
        let r' = insert r val_ in
        let cur = make_node v l r' in
        let bf = balance_factor cur in
        if bf < -1 then
          match r' with
          | Node (rv, _, _, _) ->
              let r'' = if val_ < rv then rotate_right r' else r' in
              rotate_left (make_node v l r'')
          | Nil -> cur
        else cur

let rec print_inorder = function
  | Nil -> ()
  | Node (v, l, r, _) as node ->
      print_inorder l;
      Printf.printf "%d(BF=%d) " v (balance_factor node);
      print_inorder r

let rec print_preorder = function
  | Nil -> ()
  | Node (v, l, r, _) as node ->
      Printf.printf "%d(BF=%d) " v (balance_factor node);
      print_preorder l;
      print_preorder r

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let root = ref Nil in
    for _ = 1 to n do
      let v = Scanf.scanf " %d" (fun x -> x) in
      root := insert !root v
    done;
    (try
       let new_val = Scanf.scanf " %d" (fun x -> x) in
       root := insert !root new_val
     with End_of_file -> ());
    print_inorder !root;
    print_newline ();
    print_preorder !root;
    print_newline ()
  with End_of_file -> ()
