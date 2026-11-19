(* ElGamal public-key encryption over a toy prime group. *)

let mod_pow base exp m =
  let rec loop b e acc =
    if e = 0L then acc
    else
      let new_acc =
        if Int64.rem e 2L = 1L then Int64.rem (Int64.mul acc b) m else acc
      in
      let new_b = Int64.rem (Int64.mul b b) m in
      loop new_b (Int64.div e 2L) new_acc
  in
  loop (Int64.rem base m) exp 1L

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
  | op :: rest -> (
      let vals = List.map Int64.of_string rest in
      if op = "ENC" then
        match vals with
        | [ p; g; y; m; k ] ->
            let c1 = mod_pow g k p in
            Printf.printf "%Ld %Ld" c1
              (Int64.rem (Int64.mul m (mod_pow y k p)) p)
        | _ -> ()
      else
        match vals with
        | [ p; x; c1; c2 ] ->
            Printf.printf "%Ld"
              (Int64.rem
                 (Int64.mul c2 (mod_pow c1 (Int64.sub (Int64.sub p 1L) x) p))
                 p)
        | _ -> ())
  | _ -> ()
