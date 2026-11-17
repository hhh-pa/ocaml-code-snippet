let k =
  [|
    0x428A2F98l;
    0x71374491l;
    0xB5C0FBCFl;
    0xE9B5DBA5l;
    0x3956C25Bl;
    0x59F111F1l;
    0x923F82A4l;
    0xAB1C5ED5l;
    0xD807AA98l;
    0x12835B01l;
    0x243185BEl;
    0x550C7DC3l;
    0x72BE5D74l;
    0x80DEB1FEl;
    0x9BDC06A7l;
    0xC19BF174l;
    0xE49B69C1l;
    0xEFBE4786l;
    0x0FC19DC6l;
    0x240CA1CCl;
    0x2DE92C6Fl;
    0x4A7484AAl;
    0x5CB0A9DCl;
    0x76F988DAl;
    0x983E5152l;
    0xA831C66Dl;
    0xB00327C8l;
    0xBF597FC7l;
    0xC6E00BF3l;
    0xD5A79147l;
    0x06CA6351l;
    0x14292967l;
    0x27B70A85l;
    0x2E1B2138l;
    0x4D2C6DFCl;
    0x53380D13l;
    0x650A7354l;
    0x766A0ABBl;
    0x81C2C92El;
    0x92722C85l;
    0xA2BFE8A1l;
    0xA81A664Bl;
    0xC24B8B70l;
    0xC76C51A3l;
    0xD192E819l;
    0xD6990624l;
    0xF40E3585l;
    0x106AA070l;
    0x19A4C116l;
    0x1E376C08l;
    0x2748774Cl;
    0x34B0BCB5l;
    0x391C0CB3l;
    0x4ED8AA4Al;
    0x5B9CCA4Fl;
    0x682E6FF3l;
    0x748F82EEl;
    0x78A5636Fl;
    0x84C87814l;
    0x8CC70208l;
    0x90BEFFFAl;
    0xA4506CEBl;
    0xBEF9A3F7l;
    0xC67178F2l;
  |]

let rotr x n =
  Int32.logor (Int32.shift_right_logical x n) (Int32.shift_left x (32 - n))

let compress h block off =
  let w = Array.make 64 0l in
  for i = 0 to 15 do
    let b j = Int32.of_int (Char.code block.[off + (i * 4) + j]) in
    w.(i) <-
      Int32.logor
        (Int32.logor (Int32.shift_left (b 0) 24) (Int32.shift_left (b 1) 16))
        (Int32.logor (Int32.shift_left (b 2) 8) (b 3))
  done;
  for i = 16 to 63 do
    let open Int32 in
    let s0 =
      logxor
        (logxor (rotr w.(i - 15) 7) (rotr w.(i - 15) 18))
        (shift_right_logical w.(i - 15) 3)
    in
    let s1 =
      logxor
        (logxor (rotr w.(i - 2) 17) (rotr w.(i - 2) 19))
        (shift_right_logical w.(i - 2) 10)
    in
    w.(i) <- add (add (add w.(i - 16) s0) w.(i - 7)) s1
  done;
  let a = ref h.(0)
  and b = ref h.(1)
  and c = ref h.(2)
  and d = ref h.(3)
  and e = ref h.(4)
  and f = ref h.(5)
  and g = ref h.(6)
  and hh = ref h.(7) in
  for i = 0 to 63 do
    let open Int32 in
    let s1 = logxor (logxor (rotr !e 6) (rotr !e 11)) (rotr !e 25) in
    let ch = logxor (logand !e !f) (logand (lognot !e) !g) in
    let t1 = add (add (add (add !hh s1) ch) k.(i)) w.(i) in
    let s0 = logxor (logxor (rotr !a 2) (rotr !a 13)) (rotr !a 22) in
    let maj = logxor (logxor (logand !a !b) (logand !a !c)) (logand !b !c) in
    let t2 = add s0 maj in
    hh := !g;
    g := !f;
    f := !e;
    e := add !d t1;
    d := !c;
    c := !b;
    b := !a;
    a := add t1 t2
  done;
  h.(0) <- Int32.add h.(0) !a;
  h.(1) <- Int32.add h.(1) !b;
  h.(2) <- Int32.add h.(2) !c;
  h.(3) <- Int32.add h.(3) !d;
  h.(4) <- Int32.add h.(4) !e;
  h.(5) <- Int32.add h.(5) !f;
  h.(6) <- Int32.add h.(6) !g;
  h.(7) <- Int32.add h.(7) !hh

let sha256_bytes msg =
  let h =
    [|
      0x6A09E667l;
      0xBB67AE85l;
      0x3C6EF372l;
      0xA54FF53Al;
      0x510E527Fl;
      0x9B05688Cl;
      0x1F83D9ABl;
      0x5BE0CD19l;
    |]
  in
  let ml = Bytes.length msg * 8 in
  let padded = Buffer.create ((((Bytes.length msg + 9) / 64) + 1) * 64) in
  Buffer.add_bytes padded msg;
  Buffer.add_char padded '\x80';
  while Buffer.length padded mod 64 <> 56 do
    Buffer.add_char padded '\x00'
  done;
  for i = 7 downto 0 do
    Buffer.add_char padded (Char.chr ((ml lsr (i * 8)) land 0xFF))
  done;
  let p = Buffer.to_bytes padded in
  let s = Bytes.unsafe_to_string p in
  let nblocks = Bytes.length p / 64 in
  for b = 0 to nblocks - 1 do
    compress h s (b * 64)
  done;
  let out = Buffer.create 32 in
  Array.iter
    (fun v ->
      Buffer.add_char out
        (Char.chr (Int32.to_int (Int32.shift_right_logical v 24) land 0xFF));
      Buffer.add_char out
        (Char.chr (Int32.to_int (Int32.shift_right_logical v 16) land 0xFF));
      Buffer.add_char out
        (Char.chr (Int32.to_int (Int32.shift_right_logical v 8) land 0xFF));
      Buffer.add_char out (Char.chr (Int32.to_int v land 0xFF)))
    h;
  Buffer.to_bytes out

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

let mod_mul a b m = Int64.rem (Int64.mul a b) m

let hash_mod_q msg q =
  let d = sha256_bytes msg in
  let h = ref 0L in
  for i = 0 to Bytes.length d - 1 do
    h :=
      Int64.rem
        (Int64.add (mod_mul !h 256L q)
           (Int64.of_int (Char.code (Bytes.get d i))))
        q
  done;
  !h

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

let rec ext_gcd a b =
  if b = 0L then (a, 1L, 0L)
  else
    let g, x1, y1 = ext_gcd b (Int64.rem a b) in
    (g, y1, Int64.sub x1 (Int64.mul (Int64.div a b) y1))

let mod_inv a m =
  let _, x, _ = ext_gcd (Int64.rem (Int64.add (Int64.rem a m) m) m) m in
  let res = Int64.rem x m in
  if res < 0L then Int64.add res m else res

let () =
  match split_ws (read_all ()) with
  | op :: rest -> (
      match rest with
      | ps :: qs :: gs :: ks :: mhs :: tl -> (
          let p = Int64.of_string ps
          and q = Int64.of_string qs
          and g = Int64.of_string gs in
          let key = Int64.of_string ks and msg = parse_hex mhs in
          let h = hash_mod_q msg q in
          if op = "SIGN" then
            match tl with
            | [ kks ] ->
                let k = Int64.of_string kks in
                let r = Int64.rem (mod_pow g k p) q in
                let s =
                  mod_mul (mod_inv k q)
                    (Int64.rem (Int64.add h (mod_mul key r q)) q)
                    q
                in
                Printf.printf "%Ld %Ld" r s
            | _ -> ()
          else
            match tl with
            | [ rs; ss ] ->
                let r = Int64.of_string rs and s = Int64.of_string ss in
                let ok =
                  r > 0L && r < q && s > 0L && s < q
                  &&
                  let w = mod_inv s q in
                  let u1 = mod_mul h w q and u2 = mod_mul r w q in
                  Int64.rem (mod_mul (mod_pow g u1 p) (mod_pow key u2 p) p) q
                  = r
                in
                print_string (if ok then "VALID" else "INVALID")
            | _ -> ())
      | _ -> ())
  | _ -> ()
