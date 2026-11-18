(* Ackermann function (exact closed forms for m <= 3). *)

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
  | a :: b :: _ ->
      let m = Int64.of_string a and n = Int64.of_string b in
      let open Int64 in
      if m = 0L then print_string (to_string (add n 1L))
      else if m = 1L then print_string (to_string (add n 2L))
      else if m = 2L then print_string (to_string (add (mul 2L n) 3L))
      else if m = 3L then
        print_string (to_string (sub (shift_left 1L (to_int n + 3)) 3L))
      else print_string "TOO BIG"
  | _ -> ()
