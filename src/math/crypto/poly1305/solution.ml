let mask26 = Int64.sub (Int64.shift_left 1L 26) 1L

let load_r_limbs key =
  let g i = Int64.of_int (Char.code (Bytes.get key i)) in
  let t0 =
    Int64.logor
      (Int64.logor (g 0) (Int64.shift_left (g 1) 8))
      (Int64.logor (Int64.shift_left (g 2) 16) (Int64.shift_left (g 3) 24))
  in
  let t1 =
    Int64.logor
      (Int64.logor
         (Int64.shift_right_logical (g 3) 2)
         (Int64.shift_left (g 4) 6))
      (Int64.logor (Int64.shift_left (g 5) 14) (Int64.shift_left (g 6) 22))
  in
  let t2 =
    Int64.logor
      (Int64.logor
         (Int64.shift_right_logical (g 6) 4)
         (Int64.shift_left (g 7) 4))
      (Int64.logor (Int64.shift_left (g 8) 12) (Int64.shift_left (g 9) 20))
  in
  let t3 =
    Int64.logor
      (Int64.logor
         (Int64.shift_right_logical (g 9) 6)
         (Int64.shift_left (g 10) 2))
      (Int64.logor (Int64.shift_left (g 11) 10) (Int64.shift_left (g 12) 18))
  in
  let t4 =
    Int64.logor (g 13)
      (Int64.logor (Int64.shift_left (g 14) 8) (Int64.shift_left (g 15) 16))
  in
  [|
    Int64.logand t0 mask26;
    Int64.logand (Int64.logand t1 mask26) 0x3FFFF03L;
    Int64.logand (Int64.logand t2 mask26) 0x3FFC0FFL;
    Int64.logand (Int64.logand t3 mask26) 0x3F03FFFL;
    Int64.logand (Int64.logand t4 mask26) 0x00FFFFFL;
  |]

let load_block_limbs msg off blen =
  let lo = ref 0L in
  let hi = ref 0L in
  for i = 0 to blen - 1 do
    let v = Int64.of_int (Char.code (Bytes.get msg (off + i))) in
    if i < 8 then lo := Int64.logor !lo (Int64.shift_left v (i * 8))
    else hi := Int64.logor !hi (Int64.shift_left v ((i - 8) * 8))
  done;
  let b =
    [|
      Int64.logand !lo mask26;
      Int64.logand (Int64.shift_right_logical !lo 26) mask26;
      Int64.logand
        (Int64.logor
           (Int64.shift_right_logical !lo 52)
           (Int64.shift_left !hi 12))
        mask26;
      Int64.logand (Int64.shift_right_logical !hi 14) mask26;
      Int64.logand (Int64.shift_right_logical !hi 40) mask26;
    |]
  in
  let top = blen * 8 in
  if top < 128 then
    b.(top / 26) <- Int64.logor b.(top / 26) (Int64.shift_left 1L (top mod 26))
  else b.(4) <- Int64.logor b.(4) (Int64.shift_left 1L 24);
  b

let freeze_limbs h =
  let p = [| 0x3FFFFFBL; mask26; mask26; mask26; mask26 |] in
  let g = Array.make 5 0L in
  let borrow = ref 0L in
  for i = 0 to 4 do
    if h.(i) < Int64.add p.(i) !borrow then begin
      g.(i) <-
        Int64.sub
          (Int64.add h.(i) (Int64.shift_left 1L 26))
          (Int64.add p.(i) !borrow);
      borrow := 1L
    end
    else begin
      g.(i) <- Int64.sub (Int64.sub h.(i) p.(i)) !borrow;
      borrow := 0L
    end
  done;
  if !borrow = 0L then Array.blit g 0 h 0 5

let poly_tag key msg =
  let r = load_r_limbs key in
  let s1 = Int64.mul r.(1) 5L in
  let s2 = Int64.mul r.(2) 5L in
  let s3 = Int64.mul r.(3) 5L in
  let s4 = Int64.mul r.(4) 5L in
  let h = Array.make 5 0L in
  let off = ref 0 in
  while !off < Bytes.length msg do
    let blen = min 16 (Bytes.length msg - !off) in
    let b = load_block_limbs msg !off blen in
    for i = 0 to 4 do
      h.(i) <- Int64.add h.(i) b.(i)
    done;
    let d =
      [|
        Int64.add
          (Int64.add
             (Int64.add (Int64.mul h.(0) r.(0)) (Int64.mul h.(1) s4))
             (Int64.add (Int64.mul h.(2) s3) (Int64.mul h.(3) s2)))
          (Int64.mul h.(4) s1);
        Int64.add
          (Int64.add
             (Int64.add (Int64.mul h.(0) r.(1)) (Int64.mul h.(1) r.(0)))
             (Int64.add (Int64.mul h.(2) s4) (Int64.mul h.(3) s3)))
          (Int64.mul h.(4) s2);
        Int64.add
          (Int64.add
             (Int64.add (Int64.mul h.(0) r.(2)) (Int64.mul h.(1) r.(1)))
             (Int64.add (Int64.mul h.(2) r.(0)) (Int64.mul h.(3) s4)))
          (Int64.mul h.(4) s3);
        Int64.add
          (Int64.add
             (Int64.add (Int64.mul h.(0) r.(3)) (Int64.mul h.(1) r.(2)))
             (Int64.add (Int64.mul h.(2) r.(1)) (Int64.mul h.(3) r.(0))))
          (Int64.mul h.(4) s4);
        Int64.add
          (Int64.add
             (Int64.add (Int64.mul h.(0) r.(4)) (Int64.mul h.(1) r.(3)))
             (Int64.add (Int64.mul h.(2) r.(2)) (Int64.mul h.(3) r.(1))))
          (Int64.mul h.(4) r.(0));
      |]
    in
    for i = 0 to 3 do
      let c = Int64.shift_right_logical d.(i) 26 in
      h.(i) <- Int64.logand d.(i) mask26;
      d.(i + 1) <- Int64.add d.(i + 1) c
    done;
    let c4 = Int64.shift_right_logical d.(4) 26 in
    h.(4) <- Int64.logand d.(4) mask26;
    h.(0) <- Int64.add h.(0) (Int64.mul c4 5L);
    let c0 = Int64.shift_right_logical h.(0) 26 in
    h.(0) <- Int64.logand h.(0) mask26;
    h.(1) <- Int64.add h.(1) c0;
    off := !off + 16
  done;
  freeze_limbs h;
  let out = Bytes.make 17 '\x00' in
  let acc = ref 0L in
  let bits = ref 0 in
  let idx = ref 0 in
  Array.iter
    (fun limb ->
      acc := Int64.logor !acc (Int64.shift_left limb !bits);
      bits := !bits + 26;
      while !bits >= 8 do
        Bytes.set out !idx (Char.chr (Int64.to_int (Int64.logand !acc 0xFFL)));
        acc := Int64.shift_right_logical !acc 8;
        bits := !bits - 8;
        incr idx
      done)
    h;
  let carry = ref 0 in
  for i = 0 to 15 do
    let v =
      Char.code (Bytes.get out i) + Char.code (Bytes.get key (16 + i)) + !carry
    in
    Bytes.set out i (Char.chr (v land 0xFF));
    carry := v lsr 8
  done;
  let dig = "0123456789abcdef" in
  let s = Buffer.create 32 in
  for i = 0 to 15 do
    let v = Char.code (Bytes.get out i) in
    Buffer.add_char s dig.[v lsr 4];
    Buffer.add_char s dig.[v land 0x0F]
  done;
  Buffer.contents s

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
  | keyhex :: rest ->
      let msghex = match rest with [] -> "" | m :: _ -> m in
      print_string (poly_tag (parse_hex keyhex) (parse_hex msghex))
