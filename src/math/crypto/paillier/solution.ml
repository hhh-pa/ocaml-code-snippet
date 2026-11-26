(* Paillier additive homomorphic encryption (toy modulus). *)

let mod_add a b m =
  if a >= Int64.sub m b then Int64.sub a (Int64.sub m b) else Int64.add a b

let mod_mul a b m =
  let r = ref 0L and aa = ref a and bb = ref b in
  while !bb > 0L do
    if Int64.rem !bb 2L = 1L then r := mod_add !r !aa m;
    aa := mod_add !aa !aa m;
    bb := Int64.div !bb 2L
  done;
  !r

let mod_pow base exp m =
  let rec loop b e acc =
    if e = 0L then acc
    else
      let new_acc = if Int64.rem e 2L = 1L then mod_mul acc b m else acc in
      loop (mod_mul b b m) (Int64.div e 2L) new_acc
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
        | [ n; g; m; r ] ->
            let n2 = Int64.mul n n in
            Printf.printf "%Ld" (mod_mul (mod_pow g m n2) (mod_pow r n n2) n2)
        | _ -> ()
      else if op = "DEC" then
        match vals with
        | [ n; lam; mu; c ] ->
            let n2 = Int64.mul n n in
            Printf.printf "%Ld"
              (mod_mul (Int64.div (Int64.sub (mod_pow c lam n2) 1L) n) mu n)
        | _ -> ()
      else
        match vals with
        | [ n; c1; c2 ] -> Printf.printf "%Ld" (mod_mul c1 c2 (Int64.mul n n))
        | _ -> ())
  | _ -> ()
