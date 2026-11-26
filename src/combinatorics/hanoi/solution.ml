(* Tower of Hanoi move sequence (A -> C via B). *)

let rec hn_move k src dst aux acc =
  if k = 0 then acc
  else
    let acc = hn_move (k - 1) src aux dst acc in
    let acc = (src ^ " " ^ dst) :: acc in
    hn_move (k - 1) aux dst src acc

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
      let n = int_of_string (List.hd toks) in
      let moves = List.rev (hn_move n "A" "C" "B" []) in
      print_int (List.length moves);
      List.iter (fun mv -> print_string ("\n" ^ mv)) moves
