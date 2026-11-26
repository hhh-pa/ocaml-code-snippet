(* K-way merge of sorted lists (heap, O(N log k)). *)

module T = Set.Make (struct
  type t = int * int * int

  let compare = compare
end)

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
      let k = nums.(0) in
      let lists = Array.make k [||] in
      let pos = ref 1 in
      for i = 0 to k - 1 do
        let m = nums.(!pos) in
        incr pos;
        lists.(i) <- Array.init m (fun j -> nums.(!pos + j));
        pos := !pos + m
      done;
      let heap = ref T.empty in
      for i = 0 to k - 1 do
        if Array.length lists.(i) > 0 then
          heap := T.add (lists.(i).(0), i, 0) !heap
      done;
      let out = ref [] in
      while not (T.is_empty !heap) do
        let v, i, j = T.min_elt !heap in
        heap := T.remove (v, i, j) !heap;
        out := v :: !out;
        if j + 1 < Array.length lists.(i) then
          heap := T.add (lists.(i).(j + 1), i, j + 1) !heap
      done;
      print_string (String.concat " " (List.rev_map string_of_int !out))
