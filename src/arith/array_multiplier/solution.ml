let () =
  try
    let a = Scanf.scanf " %s" (fun x -> x) in
    let b = Scanf.scanf " %s" (fun x -> x) in
    let n = max (String.length a) (String.length b) in
    let pad s = String.make (n - String.length s) '0' ^ s in
    let a = pad a and b = pad b in
    let acc = Array.make (2 * n) 0 in
    for j = 0 to n - 1 do
      if b.[n - 1 - j] = '1' then begin
        let carry = ref 0 in
        for i = 0 to n - 1 do
          let bit =
            acc.(i + j) + (Char.code a.[n - 1 - i] - Char.code '0') + !carry
          in
          acc.(i + j) <- bit land 1;
          carry := bit lsr 1
        done;
        let pos = ref (n + j) in
        while !carry <> 0 do
          let bit = acc.(!pos) + !carry in
          acc.(!pos) <- bit land 1;
          carry := bit lsr 1;
          incr pos
        done
      end
    done;
    for i = (2 * n) - 1 downto 0 do
      print_char (Char.chr (Char.code '0' + acc.(i)))
    done
  with End_of_file -> ()
