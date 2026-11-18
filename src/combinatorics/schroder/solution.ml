(* Large Schroder numbers via small-number recurrence (Brualdi 8.5). *)

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
      let nums = Array.of_list (List.map Int64.of_string toks) in
      let q = Int64.to_int nums.(0) in
      let ns = Array.init q (fun i -> Int64.to_int nums.(i + 1)) in
      let mx = Array.fold_left max 0 ns in
      let s = Array.make (mx + 2) 0L in
      s.(1) <- 1L;
      for n = 2 to mx + 1 do
        let acc = ref 0L in
        for j = 1 to n - 1 do
          acc := Int64.add !acc (Int64.mul s.(j) s.(n - j))
        done;
        s.(n) <- Int64.sub (Int64.mul 2L !acc) s.(n - 1)
      done;
      let out =
        Array.to_list
          (Array.map
             (fun n ->
               if n = 0 then "1" else Int64.to_string (Int64.mul 2L s.(n + 1)))
             ns)
      in
      print_string (String.concat "\n" out)
