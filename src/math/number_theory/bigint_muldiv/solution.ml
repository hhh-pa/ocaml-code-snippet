(* Grade-school below this many digits; Karatsuba above it. *)
let karat_threshold = 16

let strip_vec d =
  let r = ref (Array.to_list d) in
  while List.length !r > 1 && List.hd (List.rev !r) = 0 do
    r := List.rev (List.tl (List.rev !r))
  done;
  Array.of_list !r

let cmp_vec a b =
  if Array.length a <> Array.length b then
    if Array.length a < Array.length b then -1 else 1
  else
    let r = ref 0 in
    let i = ref (Array.length a - 1) in
    while !r = 0 && !i >= 0 do
      if a.(!i) <> b.(!i) then r := if a.(!i) < b.(!i) then -1 else 1;
      decr i
    done;
    !r

let add_vec a b =
  let n = max (Array.length a) (Array.length b) in
  let out = ref [] in
  let carry = ref 0 in
  for i = 0 to n - 1 do
    let v =
      (if i < Array.length a then a.(i) else 0)
      + (if i < Array.length b then b.(i) else 0)
      + !carry
    in
    out := (v mod 10) :: !out;
    carry := v / 10
  done;
  if !carry <> 0 then out := !carry :: !out;
  strip_vec (Array.of_list (List.rev !out))

let sub_vec a b =
  let out = ref [] in
  let borrow = ref 0 in
  for i = 0 to Array.length a - 1 do
    let v = a.(i) - (if i < Array.length b then b.(i) else 0) - !borrow in
    if v < 0 then begin
      out := (v + 10) :: !out;
      borrow := 1
    end
    else begin
      out := v :: !out;
      borrow := 0
    end
  done;
  strip_vec (Array.of_list (List.rev !out))

let mul_school a b =
  let out = Array.make (Array.length a + Array.length b) 0 in
  Array.iteri
    (fun i x -> Array.iteri (fun j y -> out.(i + j) <- out.(i + j) + (x * y)) b)
    a;
  let carry = ref 0 in
  for i = 0 to Array.length out - 1 do
    let v = out.(i) + !carry in
    out.(i) <- v mod 10;
    carry := v / 10
  done;
  let tail = ref [] in
  while !carry <> 0 do
    tail := (!carry mod 10) :: !tail;
    carry := !carry / 10
  done;
  strip_vec (Array.append out (Array.of_list (List.rev !tail)))

let rec mul_vec a b =
  let a, b = if Array.length a < Array.length b then (b, a) else (a, b) in
  if Array.length b < karat_threshold then mul_school a b
  else
    let m = Array.length a / 2 in
    let a0 = Array.sub a 0 m in
    let a1 = Array.sub a m (Array.length a - m) in
    let bl = min m (Array.length b) in
    let b0 = Array.sub b 0 bl in
    let b1 =
      if Array.length b > m then Array.sub b m (Array.length b - m) else [| 0 |]
    in
    let z0 = mul_vec a0 b0 in
    let z2 = mul_vec a1 b1 in
    let z1 =
      sub_vec (sub_vec (mul_vec (add_vec a0 a1) (add_vec b0 b1)) z0) z2
    in
    let t2 = Array.append (Array.make (2 * m) 0) z2 in
    let t1 = Array.append (Array.make m 0) z1 in
    strip_vec (add_vec (add_vec t2 t1) z0)

let trial_digit b rem =
  let q = ref 9 in
  while !q > 0 && cmp_vec (mul_school b [| !q |]) rem > 0 do
    decr q
  done;
  !q

let divmod_vec a b =
  if cmp_vec a b < 0 then ([| 0 |], Array.copy a)
  else
    let quot = Array.make (Array.length a) 0 in
    let rem = ref [||] in
    for i = Array.length a - 1 downto 0 do
      rem := strip_vec (Array.append [| a.(i) |] !rem);
      let q = trial_digit b !rem in
      quot.(i) <- q;
      rem := sub_vec !rem (mul_school b [| q |])
    done;
    (strip_vec quot, strip_vec !rem)

let parse_vec s =
  strip_vec
    (Array.init (String.length s) (fun i ->
         Char.code s.[String.length s - 1 - i] - Char.code '0'))

let show_vec d =
  let buf = Buffer.create (Array.length d) in
  for i = Array.length d - 1 downto 0 do
    Buffer.add_char buf (Char.chr (Char.code '0' + d.(i)))
  done;
  Buffer.contents buf

let () =
  try
    let op = Scanf.scanf " %s" (fun x -> x) in
    let a = parse_vec (Scanf.scanf " %s" (fun x -> x)) in
    let b = parse_vec (Scanf.scanf " %s" (fun x -> x)) in
    if op = "MUL" then print_string (show_vec (mul_vec a b))
    else
      let q, r = divmod_vec a b in
      print_string (show_vec q);
      print_char ' ';
      print_string (show_vec r)
  with End_of_file -> ()
