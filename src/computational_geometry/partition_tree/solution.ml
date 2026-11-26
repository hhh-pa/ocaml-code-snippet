(* partition_tree: Berg 16.1 simplicial partition + SELECTINHALFPLANE. *)

type child = { tri : (int * int) list option; v : node }
and node = Leaf of ((int * int) * int) | Inner of (child list * int)

let pt_bisect pts idx ax =
  if List.length idx <= 1 then [ idx ]
  else
    let key i =
      let x, y = List.nth pts i in
      if ax = 0 then (x, y, i) else (y, x, i)
    in
    let srt = List.sort (fun a b -> compare (key a) (key b)) idx in
    let arr = Array.of_list srt in
    let mid = (Array.length arr + 1) / 2 in
    [
      Array.to_list (Array.sub arr 0 mid);
      Array.to_list (Array.sub arr mid (Array.length arr - mid));
    ]

let pt_triangle pts idx =
  let xs = List.map (fun i -> fst (List.nth pts i)) idx in
  let ys = List.map (fun i -> snd (List.nth pts i)) idx in
  let xmin = List.fold_left min max_int xs
  and xmax = List.fold_left max min_int xs
  and ymin = List.fold_left min max_int ys
  and ymax = List.fold_left max min_int ys in
  let w = xmax - xmin and h = ymax - ymin in
  [ (xmin, ymin); (xmax + h + 1, ymin); (xmin, ymax + w + 1) ]

let rec pt_build pts idx =
  match idx with
  | [ i ] -> Leaf (List.nth pts i, 1)
  | _ ->
      let classes =
        List.concat
          (List.map (fun h -> pt_bisect pts h 1) (pt_bisect pts idx 0))
      in
      let kids =
        List.map
          (fun c ->
            let u = pt_build pts c in
            match u with
            | Leaf _ -> { tri = None; v = u }
            | Inner _ -> { tri = Some (pt_triangle pts c); v = u })
          classes
      in
      Inner (kids, List.length idx)

let pt_eval a b c (x, y) = (a * x) + (b * y) + c

let rec pt_select node a b c =
  match node with
  | Leaf (p, _) -> if pt_eval a b c p >= 0 then 1 else 0
  | Inner (kids, _) ->
      List.fold_left
        (fun acc ch ->
          match ch.tri with
          | None -> acc + pt_select ch.v a b c
          | Some t ->
              let vals = List.map (pt_eval a b c) t in
              if List.for_all (fun z -> z >= 0) vals then
                let cnt =
                  match ch.v with Leaf (_, n) -> n | Inner (_, n) -> n
                in
                acc + cnt
              else if List.exists (fun z -> z >= 0) vals then
                acc + pt_select ch.v a b c
              else acc)
        0 kids

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
  | toks ->
      let nums = Array.of_list (List.map int_of_string toks) in
      let n = nums.(0) in
      let pts =
        List.init n (fun i -> (nums.(1 + (2 * i)), nums.(2 + (2 * i))))
      in
      let qpos = 1 + (2 * n) in
      let qn = if qpos < Array.length nums then nums.(qpos) else 0 in
      let root =
        if n > 0 then Some (pt_build pts (List.init n (fun i -> i))) else None
      in
      let buf = Buffer.create 256 in
      for j = 0 to qn - 1 do
        let b = qpos + 1 + (3 * j) in
        let ans =
          match root with
          | None -> 0
          | Some r -> pt_select r nums.(b) nums.(b + 1) nums.(b + 2)
        in
        if j > 0 then Buffer.add_char buf '\n';
        Buffer.add_string buf (string_of_int ans)
      done;
      print_string (Buffer.contents buf)
