(* Lexicographic r-subsets of {1..n} (Brualdi Theorem 4.4.1). *)

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
      let n = int_of_string a and r = int_of_string b in
      let cur = Array.init r (fun i -> i + 1) in
      let last = Array.init r (fun i -> n - r + 1 + i) in
      let present = Array.make (n + 1) false in
      let buf = Buffer.create 256 in
      let count = ref 0 in
      let cont = ref true in
      while !cont do
        incr count;
        for i = 0 to r - 1 do
          if i > 0 then Buffer.add_char buf ' ';
          Buffer.add_string buf (string_of_int cur.(i))
        done;
        Buffer.add_char buf '\n';
        if cur = last then cont := false
        else begin
          Array.fill present 0 (n + 1) false;
          Array.iter (fun v -> present.(v) <- true) cur;
          let k = ref (r - 1) in
          while not (cur.(!k) + 1 <= n && not present.(cur.(!k) + 1)) do
            decr k
          done;
          cur.(!k) <- cur.(!k) + 1;
          for j = !k + 1 to r - 1 do
            cur.(j) <- cur.(j - 1) + 1
          done
        end
      done;
      Printf.printf "%d\n%s" !count (Buffer.contents buf)
  | _ -> ()
