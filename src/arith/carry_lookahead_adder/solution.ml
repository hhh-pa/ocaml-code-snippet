let () =
  try
    let a = Scanf.scanf " %s" (fun x -> x) in
    let b = Scanf.scanf " %s" (fun x -> x) in
    let n = max (String.length a) (String.length b) in
    let pad s = String.make (n - String.length s) '0' ^ s in
    let a = pad a and b = pad b in
    let bit s i = Char.code s.[n - 1 - i] - Char.code '0' in
    let g = Array.init n (fun i -> bit a i land bit b i) in
    let p = Array.init n (fun i -> bit a i lxor bit b i) in
    let c = Array.make (n + 1) 0 in
    for i = 0 to n - 1 do
      let carry = ref g.(i) in
      let prod = ref 1 in
      let j = ref i in
      while !j >= 1 && !carry = 0 do
        prod := !prod land p.(!j);
        if !prod = 1 && g.(!j - 1) = 1 then carry := 1;
        decr j
      done;
      c.(i + 1) <- !carry
    done;
    print_int c.(n);
    for i = n - 1 downto 0 do
      print_int (bit a i lxor bit b i lxor c.(i))
    done
  with End_of_file -> ()
