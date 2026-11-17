(* Shamir secret sharing: split a secret into n shares, combine any k. *)

let rec ext_gcd a b =
  if b = 0L then (a, 1L, 0L)
  else
    let g, x1, y1 = ext_gcd b (Int64.rem a b) in
    (g, y1, Int64.sub x1 (Int64.mul (Int64.div a b) y1))

let mod_inv a m =
  let _, x, _ = ext_gcd (Int64.rem (Int64.add (Int64.rem a m) m) m) m in
  let res = Int64.rem x m in
  if res < 0L then Int64.add res m else res

let mod_mul a b m = Int64.rem (Int64.mul a b) m

let shamir_eval coeffs x prime =
  List.fold_left
    (fun y c -> Int64.rem (Int64.add (mod_mul y x prime) c) prime)
    0L (List.rev coeffs)

let shamir_combine xs ys prime =
  let rec loop i acc =
    if i >= List.length xs then acc
    else
      let xi = List.nth xs i in
      let num = ref 1L and den = ref 1L in
      List.iteri
        (fun j xj ->
          if i <> j then begin
            num := mod_mul !num (Int64.rem (Int64.sub prime xj) prime) prime;
            den :=
              mod_mul !den
                (Int64.rem
                   (Int64.add (Int64.rem (Int64.sub xi xj) prime) prime)
                   prime)
                prime
          end)
        xs;
      let term =
        mod_mul (mod_mul (List.nth ys i) !num prime) (mod_inv !den prime) prime
      in
      loop (i + 1) (Int64.rem (Int64.add acc term) prime)
  in
  loop 0 0L

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
      if op = "SPLIT" then
        match vals with
        | prime :: secret :: n :: t :: cs ->
            let coeffs = secret :: cs in
            let _ = t in
            let parts = ref [] in
            let x = ref 1L in
            while !x <= n do
              parts :=
                Printf.sprintf "%Ld %Ld" !x (shamir_eval coeffs !x prime)
                :: !parts;
              x := Int64.add !x 1L
            done;
            print_string (String.concat " " (List.rev !parts))
        | _ -> ()
      else
        match vals with
        | prime :: pairs ->
            let xs = ref [] and ys = ref [] in
            let rec take = function
              | a :: b :: tl ->
                  xs := a :: !xs;
                  ys := b :: !ys;
                  take tl
              | _ -> ()
            in
            take pairs;
            Printf.printf "%Ld"
              (shamir_combine (List.rev !xs) (List.rev !ys) prime)
        | _ -> ())
  | _ -> ()
