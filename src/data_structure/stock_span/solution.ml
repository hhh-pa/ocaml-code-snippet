(* Stock span problem (monotonic stack, O(n)). *)

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
      let p = Array.sub nums 1 n in
      let spans = ref [] in
      let stack = ref [] in
      for i = 0 to n - 1 do
        let x = p.(i) in
        while !stack <> [] && p.(List.hd !stack) <= x do
          stack := List.tl !stack
        done;
        spans :=
          (match !stack with [] -> i + 1 | top :: _ -> i - top) :: !spans;
        stack := i :: !stack
      done;
      print_string (String.concat " " (List.rev_map string_of_int !spans))
