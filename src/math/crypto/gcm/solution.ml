(* AES-128 ECB/CBC/CTR modes (SP 800-38A), from-scratch AES encrypt/decrypt. *)

let sbox =
  [|
    0x63;
    0x7c;
    0x77;
    0x7b;
    0xf2;
    0x6b;
    0x6f;
    0xc5;
    0x30;
    0x01;
    0x67;
    0x2b;
    0xfe;
    0xd7;
    0xab;
    0x76;
    0xca;
    0x82;
    0xc9;
    0x7d;
    0xfa;
    0x59;
    0x47;
    0xf0;
    0xad;
    0xd4;
    0xa2;
    0xaf;
    0x9c;
    0xa4;
    0x72;
    0xc0;
    0xb7;
    0xfd;
    0x93;
    0x26;
    0x36;
    0x3f;
    0xf7;
    0xcc;
    0x34;
    0xa5;
    0xe5;
    0xf1;
    0x71;
    0xd8;
    0x31;
    0x15;
    0x04;
    0xc7;
    0x23;
    0xc3;
    0x18;
    0x96;
    0x05;
    0x9a;
    0x07;
    0x12;
    0x80;
    0xe2;
    0xeb;
    0x27;
    0xb2;
    0x75;
    0x09;
    0x83;
    0x2c;
    0x1a;
    0x1b;
    0x6e;
    0x5a;
    0xa0;
    0x52;
    0x3b;
    0xd6;
    0xb3;
    0x29;
    0xe3;
    0x2f;
    0x84;
    0x53;
    0xd1;
    0x00;
    0xed;
    0x20;
    0xfc;
    0xb1;
    0x5b;
    0x6a;
    0xcb;
    0xbe;
    0x39;
    0x4a;
    0x4c;
    0x58;
    0xcf;
    0xd0;
    0xef;
    0xaa;
    0xfb;
    0x43;
    0x4d;
    0x33;
    0x85;
    0x45;
    0xf9;
    0x02;
    0x7f;
    0x50;
    0x3c;
    0x9f;
    0xa8;
    0x51;
    0xa3;
    0x40;
    0x8f;
    0x92;
    0x9d;
    0x38;
    0xf5;
    0xbc;
    0xb6;
    0xda;
    0x21;
    0x10;
    0xff;
    0xf3;
    0xd2;
    0xcd;
    0x0c;
    0x13;
    0xec;
    0x5f;
    0x97;
    0x44;
    0x17;
    0xc4;
    0xa7;
    0x7e;
    0x3d;
    0x64;
    0x5d;
    0x19;
    0x73;
    0x60;
    0x81;
    0x4f;
    0xdc;
    0x22;
    0x2a;
    0x90;
    0x88;
    0x46;
    0xee;
    0xb8;
    0x14;
    0xde;
    0x5e;
    0x0b;
    0xdb;
    0xe0;
    0x32;
    0x3a;
    0x0a;
    0x49;
    0x06;
    0x24;
    0x5c;
    0xc2;
    0xd3;
    0xac;
    0x62;
    0x91;
    0x95;
    0xe4;
    0x79;
    0xe7;
    0xc8;
    0x37;
    0x6d;
    0x8d;
    0xd5;
    0x4e;
    0xa9;
    0x6c;
    0x56;
    0xf4;
    0xea;
    0x65;
    0x7a;
    0xae;
    0x08;
    0xba;
    0x78;
    0x25;
    0x2e;
    0x1c;
    0xa6;
    0xb4;
    0xc6;
    0xe8;
    0xdd;
    0x74;
    0x1f;
    0x4b;
    0xbd;
    0x8b;
    0x8a;
    0x70;
    0x3e;
    0xb5;
    0x66;
    0x48;
    0x03;
    0xf6;
    0x0e;
    0x61;
    0x35;
    0x57;
    0xb9;
    0x86;
    0xc1;
    0x1d;
    0x9e;
    0xe1;
    0xf8;
    0x98;
    0x11;
    0x69;
    0xd9;
    0x8e;
    0x94;
    0x9b;
    0x1e;
    0x87;
    0xe9;
    0xce;
    0x55;
    0x28;
    0xdf;
    0x8c;
    0xa1;
    0x89;
    0x0d;
    0xbf;
    0xe6;
    0x42;
    0x68;
    0x41;
    0x99;
    0x2d;
    0x0f;
    0xb0;
    0x54;
    0xbb;
    0x16;
  |]

let rcon = [| 0x01; 0x02; 0x04; 0x08; 0x10; 0x20; 0x40; 0x80; 0x1b; 0x36 |]

let xtime a =
  if a land 0x80 = 0 then (a lsl 1) land 0xff else (a lsl 1) lxor 0x1b land 0xff

let expand key =
  let rk = Array.make 176 0 in
  Array.blit key 0 rk 0 16;
  let i = ref 16 in
  while !i < 176 do
    if !i mod 16 = 0 then begin
      let b0 = rk.(!i - 4)
      and b1 = rk.(!i - 3)
      and b2 = rk.(!i - 2)
      and b3 = rk.(!i - 1) in
      let k = (!i / 16) - 1 in
      rk.(!i) <- rk.(!i - 16) lxor sbox.(b1) lxor rcon.(k);
      rk.(!i + 1) <- rk.(!i - 15) lxor sbox.(b2);
      rk.(!i + 2) <- rk.(!i - 14) lxor sbox.(b3);
      rk.(!i + 3) <- rk.(!i - 13) lxor sbox.(b0);
      i := !i + 4
    end
    else begin
      rk.(!i) <- rk.(!i - 16) lxor rk.(!i - 4);
      incr i
    end
  done;
  rk

let shift_rows st =
  let tmp = Array.copy st in
  for r = 0 to 3 do
    for c = 0 to 3 do
      st.(r + (4 * c)) <- tmp.(r + (4 * ((c + r) mod 4)))
    done
  done

let mix_cols st =
  for c = 0 to 3 do
    let a0 = st.(4 * c)
    and a1 = st.((4 * c) + 1)
    and a2 = st.((4 * c) + 2)
    and a3 = st.((4 * c) + 3) in
    let t = a0 lxor a1 lxor a2 lxor a3 in
    st.(4 * c) <- a0 lxor t lxor xtime (a0 lxor a1);
    st.((4 * c) + 1) <- a1 lxor t lxor xtime (a1 lxor a2);
    st.((4 * c) + 2) <- a2 lxor t lxor xtime (a2 lxor a3);
    st.((4 * c) + 3) <- a3 lxor t lxor xtime (a3 lxor a0)
  done

let block_enc rk blk =
  let st = Array.init 16 (fun i -> blk.(i) lxor rk.(i)) in
  for rnd = 1 to 9 do
    shift_rows st;
    for i = 0 to 15 do
      st.(i) <- sbox.(st.(i))
    done;
    mix_cols st;
    for i = 0 to 15 do
      st.(i) <- st.(i) lxor rk.((rnd * 16) + i)
    done
  done;
  shift_rows st;
  for i = 0 to 15 do
    st.(i) <- sbox.(st.(i))
  done;
  for i = 0 to 15 do
    st.(i) <- st.(i) lxor rk.(160 + i)
  done;
  st

let aes_enc key blk = block_enc (expand key) blk

let hex_val c =
  match c with
  | '0' .. '9' -> Char.code c - Char.code '0'
  | 'a' .. 'f' -> Char.code c - Char.code 'a' + 10
  | 'A' .. 'F' -> Char.code c - Char.code 'A' + 10
  | _ -> 0

let parse_hex s =
  Array.init
    (String.length s / 2)
    (fun i -> (hex_val s.[2 * i] * 16) + hex_val s.[(2 * i) + 1])

let print_hex blk =
  let dig = "0123456789abcdef" in
  Array.iter
    (fun b ->
      print_char dig.[b lsr 4];
      print_char dig.[b land 0x0F])
    blk

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

let split_fields s =
  let parts = ref [] in
  let cur = Buffer.create 16 in
  String.iter
    (fun c ->
      if c = ' ' then begin
        parts := Buffer.contents cur :: !parts;
        Buffer.clear cur
      end
      else Buffer.add_char cur c)
    s;
  parts := Buffer.contents cur :: !parts;
  List.rev !parts

(* GHASH over GF(2^128) with 64-bit halves *)
let gmul (xh, xl) (yh, yl) =
  let zh = ref Int64.zero
  and zl = ref Int64.zero
  and vh = ref yh
  and vl = ref yl in
  for i = 0 to 127 do
    let bit =
      if i < 64 then
        Int64.to_int
          (Int64.logand (Int64.shift_right_logical xh (63 - i)) Int64.one)
      else
        Int64.to_int
          (Int64.logand (Int64.shift_right_logical xl (127 - i)) Int64.one)
    in
    if bit = 1 then begin
      zh := Int64.logxor !zh !vh;
      zl := Int64.logxor !zl !vl
    end;
    let lsb = Int64.logand !vl Int64.one <> Int64.zero in
    vl :=
      Int64.logor
        (Int64.shift_right_logical !vl 1)
        (Int64.shift_left (Int64.logand !vh Int64.one) 63);
    vh := Int64.shift_right_logical !vh 1;
    if lsb then vh := Int64.logxor !vh (Int64.shift_left (Int64.of_int 0xe1) 56)
  done;
  (!zh, !zl)

let ghash h aad ct =
  let data = ref (Array.to_list aad) in
  while List.length !data mod 16 <> 0 do
    data := !data @ [ 0 ]
  done;
  data := !data @ Array.to_list ct;
  while List.length !data mod 16 <> 0 do
    data := !data @ [ 0 ]
  done;
  let arr = Array.of_list !data in
  let y = ref (Int64.zero, Int64.zero) in
  let i = ref 0 in
  while !i < Array.length arr do
    let bh = ref Int64.zero and bl = ref Int64.zero in
    for j = 0 to 7 do
      bh := Int64.logor (Int64.shift_left !bh 8) (Int64.of_int arr.(!i + j));
      bl := Int64.logor (Int64.shift_left !bl 8) (Int64.of_int arr.(!i + 8 + j))
    done;
    let yh, yl = !y in
    y := gmul (Int64.logxor yh !bh, Int64.logxor yl !bl) h;
    i := !i + 16
  done;
  let yh, yl = !y in
  gmul
    ( Int64.logxor yh (Int64.of_int (8 * Array.length aad)),
      Int64.logxor yl (Int64.of_int (8 * Array.length ct)) )
    h

let int64_pair_to_blk (h, l) =
  Array.init 16 (fun i ->
      if i < 8 then
        Int64.to_int
          (Int64.logand (Int64.shift_right_logical h ((7 - i) * 8)) 0xffL)
      else
        Int64.to_int
          (Int64.logand (Int64.shift_right_logical l ((15 - i) * 8)) 0xffL))

let gctr rk j0 data =
  let n = Array.length data in
  let out = ref [] in
  let ctr = Array.copy j0 in
  let i = ref 0 in
  while !i < n do
    (let j = ref 15 and carry = ref true in
     while !j >= 0 && !carry do
       ctr.(!j) <- (ctr.(!j) + 1) land 0xff;
       if ctr.(!j) <> 0 then carry := false;
       decr j
     done);
    let ks = block_enc rk ctr in
    let m = min 16 (n - !i) in
    for j = 0 to m - 1 do
      out := (data.(!i + j) lxor ks.(j)) :: !out
    done;
    i := !i + 16
  done;
  Array.of_list (List.rev !out)

let tag_of h s0 aad ct =
  let th, tl = ghash h aad ct in
  let sh = ref Int64.zero and sl = ref Int64.zero in
  for j = 0 to 7 do
    sh := Int64.logor (Int64.shift_left !sh 8) (Int64.of_int s0.(j));
    sl := Int64.logor (Int64.shift_left !sl 8) (Int64.of_int s0.(8 + j))
  done;
  int64_pair_to_blk (Int64.logxor th !sh, Int64.logxor tl !sl)

let () =
  match split_fields (String.trim (read_all ())) with
  | op :: ks :: ivs :: rest ->
      let rk = expand (parse_hex ks) in
      let j0 = Array.make 16 0 in
      let iv = parse_hex ivs in
      Array.blit iv 0 j0 0 12;
      j0.(15) <- 1;
      let hb = block_enc rk (Array.make 16 0) in
      let hh = ref Int64.zero and hl = ref Int64.zero in
      for j = 0 to 7 do
        hh := Int64.logor (Int64.shift_left !hh 8) (Int64.of_int hb.(j));
        hl := Int64.logor (Int64.shift_left !hl 8) (Int64.of_int hb.(8 + j))
      done;
      let h = (!hh, !hl) in
      let s0 = block_enc rk j0 in
      if op = "GCM_ENC" then begin
        let pt = match rest with p :: _ -> parse_hex p | [] -> [||] in
        let aad = match rest with _ :: a :: _ -> parse_hex a | _ -> [||] in
        let ct = gctr rk j0 pt in
        print_hex ct;
        print_char ' ';
        print_hex (tag_of h s0 aad ct)
      end
      else begin
        let ct = match rest with c :: _ -> parse_hex c | [] -> [||] in
        let tag = match rest with _ :: t :: _ -> parse_hex t | _ -> [||] in
        let aad =
          match rest with _ :: _ :: a :: _ -> parse_hex a | _ -> [||]
        in
        if tag_of h s0 aad ct = tag then print_hex (gctr rk j0 ct)
        else print_string "FAIL"
      end
  | _ -> ()
