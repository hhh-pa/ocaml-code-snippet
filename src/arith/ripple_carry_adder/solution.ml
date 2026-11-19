let () =
  try
    let a = Scanf.scanf " %s" (fun x -> x) in
    let b = Scanf.scanf " %s" (fun x -> x) in
    let n = max (String.length a) (String.length b) in
    let pad s = String.make (n - String.length s) '0' ^ s in
    let a = pad a and b = pad b in
    let out = Buffer.create (n + 1) in
    let carry = ref 0 in
    for i = n - 1 downto 0 do
      let bit =
        Char.code a.[i]
        - Char.code '0'
        + (Char.code b.[i] - Char.code '0')
        + !carry
      in
      Buffer.add_char out (Char.chr (Char.code '0' + (bit land 1)));
      carry := bit lsr 1
    done;
    Buffer.add_char out (Char.chr (Char.code '0' + !carry));
    let s = Buffer.contents out in
    for i = String.length s - 1 downto 0 do
      print_char s.[i]
    done
  with End_of_file -> ()
