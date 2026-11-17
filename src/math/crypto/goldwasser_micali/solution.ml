(* Goldwasser-Micali bit-by-bit probabilistic encryption (toy modulus). *)

let mod_mul a b m = Int64.rem (Int64.mul a b) m

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
      if op = "ENC" then
        match rest with
        | ns :: xs :: bits :: rs ->
            let n = Int64.of_string ns and x = Int64.of_string xs in
            let parts = ref [] in
            List.iter2
              (fun b r ->
                let rr = Int64.of_string r in
                let mul = if b = '1' then x else 1L in
                parts :=
                  Printf.sprintf "%Ld" (mod_mul (mod_mul rr rr n) mul n)
                  :: !parts)
              (List.init (String.length bits) (String.get bits))
              rs;
            print_string (String.concat " " (List.rev !parts))
        | _ -> ()
      else
        match rest with
        | ps :: _ :: cs ->
            let p = Int64.of_string ps in
            let e = Int64.div (Int64.sub p 1L) 2L in
            List.iter
              (fun c ->
                print_char
                  (if mod_pow (Int64.of_string c) e p = Int64.sub p 1L then '1'
                   else '0'))
              cs
        | _ -> ())
  | _ -> ()
