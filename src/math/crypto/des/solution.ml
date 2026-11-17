let ip =
  [|
    58;
    50;
    42;
    34;
    26;
    18;
    10;
    2;
    60;
    52;
    44;
    36;
    28;
    20;
    12;
    4;
    62;
    54;
    46;
    38;
    30;
    22;
    14;
    6;
    64;
    56;
    48;
    40;
    32;
    24;
    16;
    8;
    57;
    49;
    41;
    33;
    25;
    17;
    9;
    1;
    59;
    51;
    43;
    35;
    27;
    19;
    11;
    3;
    61;
    53;
    45;
    37;
    29;
    21;
    13;
    5;
    63;
    55;
    47;
    39;
    31;
    23;
    15;
    7;
  |]

let fp =
  [|
    40;
    8;
    48;
    16;
    56;
    24;
    64;
    32;
    39;
    7;
    47;
    15;
    55;
    23;
    63;
    31;
    38;
    6;
    46;
    14;
    54;
    22;
    62;
    30;
    37;
    5;
    45;
    13;
    53;
    21;
    61;
    29;
    36;
    4;
    44;
    12;
    52;
    20;
    60;
    28;
    35;
    3;
    43;
    11;
    51;
    19;
    59;
    27;
    34;
    2;
    42;
    10;
    50;
    18;
    58;
    26;
    33;
    1;
    41;
    9;
    49;
    17;
    57;
    25;
  |]

let e =
  [|
    32;
    1;
    2;
    3;
    4;
    5;
    4;
    5;
    6;
    7;
    8;
    9;
    8;
    9;
    10;
    11;
    12;
    13;
    12;
    13;
    14;
    15;
    16;
    17;
    16;
    17;
    18;
    19;
    20;
    21;
    20;
    21;
    22;
    23;
    24;
    25;
    24;
    25;
    26;
    27;
    28;
    29;
    28;
    29;
    30;
    31;
    32;
    1;
  |]

let p =
  [|
    16;
    7;
    20;
    21;
    29;
    12;
    28;
    17;
    1;
    15;
    23;
    26;
    5;
    18;
    31;
    10;
    2;
    8;
    24;
    14;
    32;
    27;
    3;
    9;
    19;
    13;
    30;
    6;
    22;
    11;
    4;
    25;
  |]

let pc1 =
  [|
    57;
    49;
    41;
    33;
    25;
    17;
    9;
    1;
    58;
    50;
    42;
    34;
    26;
    18;
    10;
    2;
    59;
    51;
    43;
    35;
    27;
    19;
    11;
    3;
    60;
    52;
    44;
    36;
    63;
    55;
    47;
    39;
    31;
    23;
    15;
    7;
    62;
    54;
    46;
    38;
    30;
    22;
    14;
    6;
    61;
    53;
    45;
    37;
    29;
    21;
    13;
    5;
    28;
    20;
    12;
    4;
  |]

let pc2 =
  [|
    14;
    17;
    11;
    24;
    1;
    5;
    3;
    28;
    15;
    6;
    21;
    10;
    23;
    19;
    12;
    4;
    26;
    8;
    16;
    7;
    27;
    20;
    13;
    2;
    41;
    52;
    31;
    37;
    47;
    55;
    30;
    40;
    51;
    45;
    33;
    48;
    44;
    49;
    39;
    56;
    34;
    53;
    46;
    42;
    50;
    36;
    29;
    32;
  |]

let shifts = [| 1; 1; 2; 2; 2; 2; 2; 2; 1; 2; 2; 2; 2; 2; 2; 1 |]

let sbox =
  [|
    [|
      14;
      4;
      13;
      1;
      2;
      15;
      11;
      8;
      3;
      10;
      6;
      12;
      5;
      9;
      0;
      7;
      0;
      15;
      7;
      4;
      14;
      2;
      13;
      1;
      10;
      6;
      12;
      11;
      9;
      5;
      3;
      8;
      4;
      1;
      14;
      8;
      13;
      6;
      2;
      11;
      15;
      12;
      9;
      7;
      3;
      10;
      5;
      0;
      15;
      12;
      8;
      2;
      4;
      9;
      1;
      7;
      5;
      11;
      3;
      14;
      10;
      0;
      6;
      13;
    |];
    [|
      15;
      1;
      8;
      14;
      6;
      11;
      3;
      4;
      9;
      7;
      2;
      13;
      12;
      0;
      5;
      10;
      3;
      13;
      4;
      7;
      15;
      2;
      8;
      14;
      12;
      0;
      1;
      10;
      6;
      9;
      11;
      5;
      0;
      14;
      7;
      11;
      10;
      4;
      13;
      1;
      5;
      8;
      12;
      6;
      9;
      3;
      2;
      15;
      13;
      8;
      10;
      1;
      3;
      15;
      4;
      2;
      11;
      6;
      7;
      12;
      0;
      5;
      14;
      9;
    |];
    [|
      10;
      0;
      9;
      14;
      6;
      3;
      15;
      5;
      1;
      13;
      12;
      7;
      11;
      4;
      2;
      8;
      13;
      7;
      0;
      9;
      3;
      4;
      6;
      10;
      2;
      8;
      5;
      14;
      12;
      11;
      15;
      1;
      13;
      6;
      4;
      9;
      8;
      15;
      3;
      0;
      11;
      1;
      2;
      12;
      5;
      10;
      14;
      7;
      1;
      10;
      13;
      0;
      6;
      9;
      8;
      7;
      4;
      15;
      14;
      3;
      11;
      5;
      2;
      12;
    |];
    [|
      7;
      13;
      14;
      3;
      0;
      6;
      9;
      10;
      1;
      2;
      8;
      5;
      11;
      12;
      4;
      15;
      13;
      8;
      11;
      5;
      6;
      15;
      0;
      3;
      4;
      7;
      2;
      12;
      1;
      10;
      14;
      9;
      10;
      6;
      9;
      0;
      12;
      11;
      7;
      13;
      15;
      1;
      3;
      14;
      5;
      2;
      8;
      4;
      3;
      15;
      0;
      6;
      10;
      1;
      13;
      8;
      9;
      4;
      5;
      11;
      12;
      7;
      2;
      14;
    |];
    [|
      2;
      12;
      4;
      1;
      7;
      10;
      11;
      6;
      8;
      5;
      3;
      15;
      13;
      0;
      14;
      9;
      14;
      11;
      2;
      12;
      4;
      7;
      13;
      1;
      5;
      0;
      15;
      10;
      3;
      9;
      8;
      6;
      4;
      2;
      1;
      11;
      10;
      13;
      7;
      8;
      15;
      9;
      12;
      5;
      6;
      3;
      0;
      14;
      11;
      8;
      12;
      7;
      1;
      14;
      2;
      13;
      6;
      15;
      0;
      9;
      10;
      4;
      5;
      3;
    |];
    [|
      12;
      1;
      10;
      15;
      9;
      2;
      6;
      8;
      0;
      13;
      3;
      4;
      14;
      7;
      5;
      11;
      10;
      15;
      4;
      2;
      7;
      12;
      9;
      5;
      6;
      1;
      13;
      14;
      0;
      11;
      3;
      8;
      9;
      14;
      15;
      5;
      2;
      8;
      12;
      3;
      7;
      0;
      4;
      10;
      1;
      13;
      11;
      6;
      4;
      3;
      2;
      12;
      9;
      5;
      15;
      10;
      11;
      14;
      1;
      7;
      6;
      0;
      8;
      13;
    |];
    [|
      4;
      11;
      2;
      14;
      15;
      0;
      8;
      13;
      3;
      12;
      9;
      7;
      5;
      10;
      6;
      1;
      13;
      0;
      11;
      7;
      4;
      9;
      1;
      10;
      14;
      3;
      5;
      12;
      2;
      15;
      8;
      6;
      1;
      4;
      11;
      13;
      12;
      3;
      7;
      14;
      10;
      15;
      6;
      8;
      0;
      5;
      9;
      2;
      6;
      11;
      13;
      8;
      1;
      4;
      10;
      7;
      9;
      5;
      0;
      15;
      14;
      2;
      3;
      12;
    |];
    [|
      13;
      2;
      8;
      4;
      6;
      15;
      11;
      1;
      10;
      9;
      3;
      14;
      5;
      0;
      12;
      7;
      1;
      15;
      13;
      8;
      10;
      3;
      7;
      4;
      12;
      5;
      6;
      11;
      0;
      14;
      9;
      2;
      7;
      11;
      4;
      1;
      9;
      12;
      14;
      2;
      0;
      6;
      10;
      13;
      15;
      3;
      5;
      8;
      2;
      1;
      14;
      7;
      4;
      10;
      8;
      13;
      15;
      12;
      9;
      0;
      3;
      5;
      6;
      11;
    |];
  |]

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

let permute bits table =
  Array.init (Array.length table) (fun i -> bits.(table.(i) - 1))

let key_schedule key =
  let bits = Array.make 64 0 in
  for i = 0 to 7 do
    for b = 7 downto 0 do
      bits.((i * 8) + (7 - b)) <- (Char.code (Bytes.get key i) lsr b) land 1
    done
  done;
  let cd = permute bits pc1 in
  let c = ref (Array.sub cd 0 28) in
  let d = ref (Array.sub cd 28 28) in
  let subs = Array.make 16 [||] in
  for r = 0 to 15 do
    let s = shifts.(r) in
    let nc = Array.init 28 (fun i -> !c.((i + s) mod 28)) in
    let nd = Array.init 28 (fun i -> !d.((i + s) mod 28)) in
    c := nc;
    d := nd;
    subs.(r) <- permute (Array.append !c !d) pc2
  done;
  subs

let feistel r sub =
  let e = permute r e in
  let s = Array.make 32 0 in
  for i = 0 to 7 do
    let b j = e.((6 * i) + j) lxor sub.((6 * i) + j) in
    let v =
      sbox.(i).((b 0 * 32)
                + (b 5 * 16)
                + (b 1 * 8)
                + (b 2 * 4)
                + (b 3 * 2)
                + b 4)
    in
    s.(4 * i) <- (v lsr 3) land 1;
    s.((4 * i) + 1) <- (v lsr 2) land 1;
    s.((4 * i) + 2) <- (v lsr 1) land 1;
    s.((4 * i) + 3) <- v land 1
  done;
  permute s p

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
      let subs = key_schedule (parse_hex kh) in
      let msg = parse_hex mh in
      let bits = Array.make 64 0 in
      for i = 0 to 7 do
        for b = 7 downto 0 do
          bits.((i * 8) + (7 - b)) <- (Char.code (Bytes.get msg i) lsr b) land 1
        done
      done;
      let lr = permute bits ip in
      let l = ref (Array.sub lr 0 32) in
      let r = ref (Array.sub lr 32 32) in
      for round = 0 to 15 do
        let f = feistel !r subs.(round) in
        let nr = Array.init 32 (fun i -> !l.(i) lxor f.(i)) in
        l := !r;
        r := nr
      done;
      let out = permute (Array.append !r !l) fp in
      let dig = "0123456789abcdef" in
      for i = 0 to 63 do
        if i mod 8 = 0 then begin
          let v = ref 0 in
          for j = 0 to 7 do
            v := (!v * 2) + out.(i + j)
          done;
          print_char dig.[!v lsr 4];
          print_char dig.[!v land 0x0F]
        end
      done
  | _ -> ()
