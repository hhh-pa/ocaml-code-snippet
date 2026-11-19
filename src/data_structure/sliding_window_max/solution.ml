(* https://leetcode.com/problems/sliding-window-maximum/
   Sliding window maximum (monotonic deque, O(n)). *)

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
      let n = nums.(0) and k = nums.(1) in
      let a = Array.sub nums 2 n in
      let dq = Array.make n 0 in
      let head = ref 0 and tail = ref 0 in
      let out = ref [] in
      for i = 0 to n - 1 do
        let x = a.(i) in
        while !head < !tail && a.(dq.(!tail - 1)) <= x do
          decr tail
        done;
        dq.(!tail) <- i;
        incr tail;
        while dq.(!head) <= i - k do
          incr head
        done;
        if i + 1 >= k then out := a.(dq.(!head)) :: !out
      done;
      print_string (String.concat " " (List.rev_map string_of_int !out))
