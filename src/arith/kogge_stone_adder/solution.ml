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
    let stride = ref 1 in
    while !stride < n do
      let go = Array.copy g and po = Array.copy p in
      for i = !stride to n - 1 do
        g.(i) <- go.(i) lor (po.(i) land go.(i - !stride));
        p.(i) <- po.(i) land po.(i - !stride)
      done;
      stride := !stride * 2
    done;
    print_int g.(n - 1);
    for i = n - 1 downto 0 do
      let carry = if i = 0 then 0 else g.(i - 1) in
      print_int (bit a i lxor bit b i lxor carry)
    done
  with End_of_file -> ()
