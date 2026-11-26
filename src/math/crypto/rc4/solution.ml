let hex_val c =
  match c with
  | '0' .. '9' -> Char.code c - Char.code '0'
  | 'a' .. 'f' -> Char.code c - Char.code 'a' + 10
  | 'A' .. 'F' -> Char.code c - Char.code 'A' + 10
  | _ -> 0

let parse_hex s =
  let n = String.length s / 2 in
  let b = Bytes.create n in
  for i = 0 to n - 1 do
    Bytes.set b i
      (Char.chr ((hex_val s.[2 * i] * 16) + hex_val s.[(2 * i) + 1]))
  done;
  b

let hex_str b =
  let dig = "0123456789abcdef" in
  let s = Buffer.create (Bytes.length b * 2) in
  Bytes.iter
    (fun c ->
      let v = Char.code c in
      Buffer.add_char s dig.[v lsr 4];
      Buffer.add_char s dig.[v land 0x0F])
    b;
  Buffer.contents s

let rc4_crypt key data =
  let s = Array.init 256 (fun i -> i) in
  let j = ref 0 in
  for i = 0 to 255 do
    j :=
      (!j + s.(i) + Char.code (Bytes.get key (i mod Bytes.length key))) mod 256;
    let t = s.(i) in
    s.(i) <- s.(!j);
    s.(!j) <- t
  done;
  let i = ref 0 in
  j := 0;
  let out = Bytes.create (Bytes.length data) in
  for k = 0 to Bytes.length data - 1 do
    i := (!i + 1) mod 256;
    j := (!j + s.(!i)) mod 256;
    let t = s.(!i) in
    s.(!i) <- s.(!j);
    s.(!j) <- t;
    Bytes.set out k
      (Char.chr
         (Char.code (Bytes.get data k) lxor s.((s.(!i) + s.(!j)) mod 256)))
  done;
  out

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
  | kh :: rest ->
      let mh = match rest with [] -> "" | m :: _ -> m in
      print_string (hex_str (rc4_crypt (parse_hex kh) (parse_hex mh)))
  | [] -> ()
