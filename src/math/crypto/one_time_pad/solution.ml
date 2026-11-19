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
  | kh :: mh :: _ ->
      let key = parse_hex kh in
      let msg = parse_hex mh in
      let n = min (Bytes.length key) (Bytes.length msg) in
      let out = Bytes.create n in
      for i = 0 to n - 1 do
        Bytes.set out i
          (Char.chr
             (Char.code (Bytes.get key i) lxor Char.code (Bytes.get msg i)))
      done;
      print_string (hex_str out)
  | _ -> ()
