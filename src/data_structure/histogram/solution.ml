(* https://www.hackerrank.com/challenges/largest-rectangle/problem
   Largest rectangle in a histogram (monotonic stack, O(n)). *)

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
      let h = Array.make (n + 1) 0 in
      Array.blit nums 1 h 0 n;
      let best = ref 0 in
      let stack = ref [] in
      for i = 0 to n do
        let x = h.(i) in
        while !stack <> [] && h.(List.hd !stack) > x do
          let height = h.(List.hd !stack) in
          stack := List.tl !stack;
          let width = match !stack with [] -> i | top :: _ -> i - top - 1 in
          best := max !best (height * width)
        done;
        stack := i :: !stack
      done;
      print_int !best
