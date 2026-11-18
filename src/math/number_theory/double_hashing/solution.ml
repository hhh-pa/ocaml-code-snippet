(* Open-addressing hash table with double hashing (CLRS 11.4). *)

let dh_mod a m =
  let r = a mod m in
  if r < 0 then r + m else r

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
      let m = nums.(0) and n = nums.(1) in
      let table = Array.make m None in
      for i = 0 to n - 1 do
        let k = nums.(2 + i) in
        let h1 = dh_mod k m and h2 = 1 + dh_mod k (m - 1) in
        let t = ref 0 in
        let placed = ref false in
        while !t < m && not !placed do
          let j = dh_mod (h1 + (!t * h2)) m in
          match table.(j) with
          | None ->
              table.(j) <- Some k;
              placed := true
          | Some _ -> incr t
        done
      done;
      let parts = ref [] in
      for i = 0 to m - 1 do
        parts :=
          (match table.(i) with None -> "_" | Some v -> string_of_int v)
          :: !parts
      done;
      print_string (String.concat " " (List.rev !parts))
