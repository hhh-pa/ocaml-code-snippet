(* N-queens enumeration (row-by-row backtracking, lex order). *)

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
      let cols = Array.make n false
      and d1 = Array.make ((2 * n) + 1) false
      and d2 = Array.make ((2 * n) + 1) false in
      let cur = ref [] in
      let out = ref [] in
      let rec place r =
        if r = n then begin
          let parts = List.rev_map string_of_int !cur in
          out := String.concat " " parts :: !out
        end
        else
          for c = 0 to n - 1 do
            if (not cols.(c)) && (not d1.(r - c + n)) && not d2.(r + c) then begin
              cols.(c) <- true;
              d1.(r - c + n) <- true;
              d2.(r + c) <- true;
              cur := c :: !cur;
              place (r + 1);
              cur := List.tl !cur;
              cols.(c) <- false;
              d1.(r - c + n) <- false;
              d2.(r + c) <- false
            end
          done
      in
      place 0;
      let sols = List.rev !out in
      print_int (List.length sols);
      List.iter (fun ln -> print_string ("\n" ^ ln)) sols
