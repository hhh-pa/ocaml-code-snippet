(* https://www.hackerrank.com/challenges/binary-search-tree-insertion/problem *)
type tree = Empty | Node of int * tree * tree

let rec insert t v =
  match t with
  | Empty -> Node (v, Empty, Empty)
  | Node (root, l, r) ->
      if v <= root then Node (root, insert l v, r)
      else Node (root, l, insert r v)

let rec preorder f t =
  match t with
  | Empty -> ()
  | Node (root, l, r) ->
      f root;
      preorder f l;
      preorder f r

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let vals = Array.init (max n 0) (fun _ -> Scanf.scanf " %d" (fun x -> x)) in
    let tree = Array.fold_left (fun t v -> insert t v) Empty vals in
    preorder
      (fun v ->
        print_int v;
        print_string " ")
      tree
  with End_of_file -> ()
