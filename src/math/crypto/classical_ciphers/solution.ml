let freq =
  [|
    8.2;
    1.5;
    2.8;
    4.3;
    13.0;
    2.2;
    2.0;
    6.1;
    7.0;
    0.15;
    0.77;
    4.0;
    2.4;
    6.7;
    7.5;
    1.9;
    0.095;
    6.0;
    6.3;
    9.1;
    2.8;
    0.98;
    2.4;
    0.15;
    2.0;
    0.074;
  |]

let caesar text shift =
  let buf = Buffer.create (String.length text) in
  String.iter
    (fun c ->
      if c >= 'A' && c <= 'Z' then
        Buffer.add_char buf
          (Char.chr
             (Char.code 'A'
             + ((((Char.code c - Char.code 'A' + shift) mod 26) + 26) mod 26)))
      else if c >= 'a' && c <= 'z' then
        Buffer.add_char buf
          (Char.chr
             (Char.code 'a'
             + ((((Char.code c - Char.code 'a' + shift) mod 26) + 26) mod 26)))
      else Buffer.add_char buf c)
    text;
  Buffer.contents buf

let vigenere text key decode =
  let ks =
    List.filter_map
      (fun c ->
        let u = Char.uppercase_ascii c in
        if u >= 'A' && u <= 'Z' then Some ((Char.code u - Char.code 'A') mod 26)
        else None)
      (List.init (String.length key) (String.get key))
  in
  let buf = Buffer.create (String.length text) in
  let j = ref 0 in
  String.iter
    (fun c ->
      if (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') then begin
        let s = List.nth ks (!j mod List.length ks) in
        let s = if decode then -s else s in
        let base =
          if c >= 'A' && c <= 'Z' then Char.code 'A' else Char.code 'a'
        in
        Buffer.add_char buf
          (Char.chr (base + ((((Char.code c - base + s) mod 26) + 26) mod 26)));
        incr j
      end
      else Buffer.add_char buf c)
    text;
  Buffer.contents buf

let caesar_break text =
  let letters =
    String.concat ""
      (List.filter_map
         (fun c ->
           if (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') then
             Some (String.make 1 (Char.uppercase_ascii c))
           else None)
         (List.init (String.length text) (String.get text)))
  in
  let n = float_of_int (String.length letters) in
  let best = ref (-1.0) in
  let bests = ref 0 in
  for shift = 0 to 25 do
    let pt = caesar letters (-shift) in
    let obs = Array.make 26 0.0 in
    String.iter
      (fun c ->
        obs.(Char.code c - Char.code 'A') <-
          obs.(Char.code c - Char.code 'A') +. 1.0)
      pt;
    let chi = ref 0.0 in
    for k = 0 to 25 do
      let e = n *. freq.(k) /. 100.0 in
      chi := !chi +. ((obs.(k) -. e) *. (obs.(k) -. e) /. e)
    done;
    if !best < 0.0 || !chi < !best then begin
      best := !chi;
      bests := shift
    end
  done;
  string_of_int !bests ^ " " ^ caesar text (- !bests)

let read_all () =
  let buf = Buffer.create 256 in
  (try
     while true do
       Buffer.add_char buf (input_char stdin)
     done
   with End_of_file -> ());
  Buffer.contents buf

let is_space = function ' ' | '\t' | '\n' | '\r' -> true | _ -> false

let split_head s =
  let n = String.length s in
  let i = ref 0 in
  while !i < n && is_space s.[!i] do
    incr i
  done;
  let j = ref !i in
  while !j < n && not (is_space s.[!j]) do
    incr j
  done;
  let tok = String.sub s !i (!j - !i) in
  let k = ref !j in
  while !k < n && is_space s.[!k] do
    incr k
  done;
  (tok, String.sub s !k (n - !k))

let () =
  let op, rest = split_head (read_all ()) in
  if op = "" then ()
  else if op = "BREAK" then print_string (caesar_break rest)
  else
    let arg, text = split_head rest in
    if op = "CAESAR_ENC" then print_string (caesar text (int_of_string arg))
    else if op = "CAESAR_DEC" then
      print_string (caesar text (-int_of_string arg))
    else if op = "VIGENERE_ENC" then print_string (vigenere text arg false)
    else print_string (vigenere text arg true)
