let add_sub a b sub =
  let n = Array.length a in
  let out = Array.make n 0 in
  let carry = ref (if sub then 1 else 0) in
  for i = 0 to n - 1 do
    let bb = if sub then 1 - b.(i) else b.(i) in
    let bit = a.(i) + bb + !carry in
    out.(i) <- bit land 1;
    carry := bit lsr 1
  done;
  (out, !carry)

let parse_amount b n =
  let k = ref 0 in
  String.iter
    (fun c -> if !k < n then k := (!k * 2) + (Char.code c - Char.code '0'))
    b;
  !k

let execute op ab bb b n =
  if op = "ADD" || op = "SUB" then add_sub ab bb (op = "SUB")
  else if op = "AND" || op = "OR" || op = "XOR" then (
    let res = Array.make n 0 in
    for i = 0 to n - 1 do
      res.(i) <-
        (if op = "AND" then ab.(i) land bb.(i)
         else if op = "OR" then ab.(i) lor bb.(i)
         else ab.(i) lxor bb.(i))
    done;
    (res, 0))
  else
    let k = parse_amount b n in
    let res = Array.make n 0 in
    (if op = "SHL" then
       for i = 0 to n - 1 do
         if i + k < n then res.(i + k) <- ab.(i)
       done
     else
       let sign = ab.(n - 1) in
       for i = 0 to n - 1 do
         res.(i) <- (if i + k < n then ab.(i + k) else sign)
       done);
    (res, 0)

let () =
  try
    let op = Scanf.scanf " %s" (fun x -> x) in
    let a = Scanf.scanf " %s" (fun x -> x) in
    let b = Scanf.scanf " %s" (fun x -> x) in
    let n = max (String.length a) (String.length b) in
    let pad s = String.make (n - String.length s) '0' ^ s in
    let a = pad a and b = pad b in
    let va = Array.init n (fun i -> Char.code a.[n - 1 - i] - Char.code '0') in
    let vb = Array.init n (fun i -> Char.code b.[n - 1 - i] - Char.code '0') in
    let res, carry = execute op va vb b n in
    for i = n - 1 downto 0 do
      print_char (Char.chr (Char.code '0' + res.(i)))
    done;
    print_char ' ';
    print_int carry;
    print_int (if Array.for_all (fun v -> v = 0) res then 1 else 0);
    print_int res.(n - 1)
  with End_of_file -> ()
