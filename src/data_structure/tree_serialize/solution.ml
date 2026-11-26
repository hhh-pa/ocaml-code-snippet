(* Binary-tree codec: preorder-with-nulls in, trimmed level-order out. *)

type tnode = Empty | Node of int * tnode * tnode

let rec ts_build toks pos =
  if toks.(pos) = "#" then (Empty, pos + 1)
  else
    let v = int_of_string toks.(pos) in
    let l, p1 = ts_build toks (pos + 1) in
    let r, p2 = ts_build toks p1 in
    (Node (v, l, r), p2)

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
      let all = Array.of_list toks in
      let n = int_of_string all.(0) in
      let pre = Array.sub all 1 n in
      let root, _ = ts_build pre 0 in
      let q = Queue.create () in
      Queue.add root q;
      let out = ref [] in
      while not (Queue.is_empty q) do
        match Queue.take q with
        | Empty -> out := "#" :: !out
        | Node (v, l, r) ->
            out := string_of_int v :: !out;
            Queue.add l q;
            Queue.add r q
      done;
      let rec drop = function "#" :: rest -> drop rest | l -> l in
      let trimmed = List.rev (drop !out) in
      print_string (String.concat " " trimmed)
