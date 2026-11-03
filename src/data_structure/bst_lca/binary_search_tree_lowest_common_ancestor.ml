type node =
  | Nil
  | Node of int * node * node

let rec insert root data =
  match root with
  | Nil -> Node (data, Nil, Nil)
  | Node (v, l, r) ->
      if data <= v then
        Node (v, insert l data, r)
      else
        Node (v, l, insert r data)

let rec lca root v1 v2 =
  match root with
  | Nil -> None
  | Node (v, l, r) ->
      if v1 < v && v2 < v then
        lca l v1 v2
      else if v1 > v && v2 > v then
        lca r v1 v2
      else
        Some v

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let root = ref Nil in
    for _ = 1 to n do
      let v = Scanf.scanf " %d" (fun x -> x) in
      root := insert !root v
    done;
    let v1 = Scanf.scanf " %d" (fun x -> x) in
    let v2 = Scanf.scanf " %d" (fun x -> x) in
    match lca !root v1 v2 with
    | Some ans -> Printf.printf "%d\n" ans
    | None -> ()
  with End_of_file -> ()
