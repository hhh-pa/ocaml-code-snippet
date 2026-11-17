let subtract_bits a b =
  let n = Array.length a in
  let diff = Array.make n 0 in
  let carry = ref 1 in
  for i = 0 to n - 1 do
    let bit = a.(i) + (1 - b.(i)) + !carry in
    diff.(i) <- bit land 1;
    carry := bit lsr 1
  done;
  (diff, !carry)

let () =
  try
    let a = Scanf.scanf " %s" (fun x -> x) in
    let b = Scanf.scanf " %s" (fun x -> x) in
    let n = max (String.length a) (String.length b) in
    let pad s = String.make (n - String.length s) '0' ^ s in
    let a = pad a and b = pad b in
    let va = Array.init n (fun i -> Char.code a.[n - 1 - i] - Char.code '0') in
    let vb = Array.init n (fun i -> Char.code b.[n - 1 - i] - Char.code '0') in
    let diff, ge = subtract_bits va vb in
    let _, le = subtract_bits vb va in
    let eq = if Array.for_all (fun d -> d = 0) diff then 1 else 0 in
    print_int ge;
    print_int eq;
    print_int le
  with End_of_file -> ()
