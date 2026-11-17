let negate_bits bits =
  let out = Array.map (fun x -> 1 - x) bits in
  let carry = ref 1 in
  for i = 0 to Array.length out - 1 do
    let bit = out.(i) + !carry in
    out.(i) <- bit land 1;
    carry := bit lsr 1
  done;
  out

let add_bits a b =
  let out = Array.make (Array.length a) 0 in
  let carry = ref 0 in
  for i = 0 to Array.length out - 1 do
    let bit = a.(i) + b.(i) + !carry in
    out.(i) <- bit land 1;
    carry := bit lsr 1
  done;
  out

let () =
  try
    let a = Scanf.scanf " %s" (fun x -> x) in
    let b = Scanf.scanf " %s" (fun x -> x) in
    let n = max (String.length a) (String.length b) in
    let pad s = String.make (n - String.length s) '0' ^ s in
    let a = pad a and b = pad b in
    let m = Array.init n (fun i -> Char.code a.[n - 1 - i] - Char.code '0') in
    let y = Array.init n (fun i -> Char.code b.[n - 1 - i] - Char.code '0') in
    let width = (2 * n) + 2 in
    let acc = ref (Array.make width 0) in
    let groups = (n + 2) / 2 in
    let bit_at v j = if j >= 0 && j < n then v.(j) else 0 in
    for k = 0 to groups - 1 do
      let sel =
        (bit_at y ((2 * k) + 1) lsl 2)
        lor (bit_at y (2 * k) lsl 1)
        lor bit_at y ((2 * k) - 1)
      in
      if sel <> 0 && sel <> 7 then begin
        let pp = Array.make width 0 in
        let off = if sel = 3 || sel = 4 then (2 * k) + 1 else 2 * k in
        for i = 0 to n - 1 do
          pp.(off + i) <- m.(i)
        done;
        let pp = if sel >= 4 then negate_bits pp else pp in
        acc := add_bits !acc pp
      end
    done;
    for i = (2 * n) - 1 downto 0 do
      print_char (Char.chr (Char.code '0' + !acc.(i)))
    done
  with End_of_file -> ()
