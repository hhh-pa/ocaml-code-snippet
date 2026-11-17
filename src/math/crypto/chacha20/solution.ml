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

let rotl32 x n =
  Int32.logor (Int32.shift_left x n) (Int32.shift_right_logical x (32 - n))

let quarter_round w a b c d =
  w.(a) <- Int32.add w.(a) w.(b);
  w.(d) <- Int32.logxor w.(d) w.(a);
  w.(d) <- rotl32 w.(d) 16;
  w.(c) <- Int32.add w.(c) w.(d);
  w.(b) <- Int32.logxor w.(b) w.(c);
  w.(b) <- rotl32 w.(b) 12;
  w.(a) <- Int32.add w.(a) w.(b);
  w.(d) <- Int32.logxor w.(d) w.(a);
  w.(d) <- rotl32 w.(d) 8;
  w.(c) <- Int32.add w.(c) w.(d);
  w.(b) <- Int32.logxor w.(b) w.(c);
  w.(b) <- rotl32 w.(b) 7

let le32 b o =
  Int32.logor
    (Int32.logor
       (Int32.of_int (Char.code (Bytes.get b o)))
       (Int32.shift_left (Int32.of_int (Char.code (Bytes.get b (o + 1)))) 8))
    (Int32.logor
       (Int32.shift_left (Int32.of_int (Char.code (Bytes.get b (o + 2)))) 16)
       (Int32.shift_left (Int32.of_int (Char.code (Bytes.get b (o + 3)))) 24))

let chacha_block key counter nonce =
  let st =
    [|
      0x61707865l;
      0x3320646El;
      0x79622D32l;
      0x6B206574l;
      0l;
      0l;
      0l;
      0l;
      0l;
      0l;
      0l;
      0l;
      0l;
      0l;
      0l;
      0l;
    |]
  in
  for i = 0 to 7 do
    st.(4 + i) <- le32 key (4 * i)
  done;
  st.(12) <- counter;
  for i = 0 to 2 do
    st.(13 + i) <- le32 nonce (4 * i)
  done;
  let w = Array.copy st in
  for _ = 0 to 9 do
    quarter_round w 0 4 8 12;
    quarter_round w 1 5 9 13;
    quarter_round w 2 6 10 14;
    quarter_round w 3 7 11 15;
    quarter_round w 0 5 10 15;
    quarter_round w 1 6 11 12;
    quarter_round w 2 7 8 13;
    quarter_round w 3 4 9 14
  done;
  let out = Bytes.create 64 in
  for i = 0 to 15 do
    let v = Int32.add w.(i) st.(i) in
    Bytes.set out (4 * i) (Char.chr (Int32.to_int v land 0xFF));
    Bytes.set out
      ((4 * i) + 1)
      (Char.chr (Int32.to_int (Int32.shift_right_logical v 8) land 0xFF));
    Bytes.set out
      ((4 * i) + 2)
      (Char.chr (Int32.to_int (Int32.shift_right_logical v 16) land 0xFF));
    Bytes.set out
      ((4 * i) + 3)
      (Char.chr (Int32.to_int (Int32.shift_right_logical v 24) land 0xFF))
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
  | kh :: nh :: cs :: rest ->
      let key = parse_hex kh in
      let nonce = parse_hex nh in
      let counter = Int64.to_int32 (Int64.of_string cs) in
      let mh = match rest with [] -> "" | m :: _ -> m in
      let msg = parse_hex mh in
      let nblocks = (Bytes.length msg + 63) / 64 in
      let out = Buffer.create (Bytes.length msg * 2) in
      let dig = "0123456789abcdef" in
      for blk = 0 to nblocks - 1 do
        let ks =
          chacha_block key (Int32.add counter (Int32.of_int blk)) nonce
        in
        let stop = min (Bytes.length msg) ((blk * 64) + 64) in
        for i = blk * 64 to stop - 1 do
          let c =
            Char.code (Bytes.get msg i)
            lxor Char.code (Bytes.get ks (i - (blk * 64)))
          in
          Buffer.add_char out dig.[c lsr 4];
          Buffer.add_char out dig.[c land 0x0F]
        done
      done;
      print_string (Buffer.contents out)
  | _ -> ()
