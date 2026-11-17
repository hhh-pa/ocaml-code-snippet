let op (g, p) (g2, p2) =
  ( (if g = 1 || (p = 1 && g2 = 1) then 1 else 0),
    if p = 1 && p2 = 1 then 1 else 0 )

let () =
  try
    let a = Scanf.scanf " %s" (fun x -> x) in
    let b = Scanf.scanf " %s" (fun x -> x) in
    let n = max (String.length a) (String.length b) in
    let pad s = String.make (n - String.length s) '0' ^ s in
    let a = pad a and b = pad b in
    let bit s i = Char.code s.[n - 1 - i] - Char.code '0' in
    let size = ref 1 in
    while !size < n do
      size := !size * 2
    done;
    let size = !size in
    let gp = Array.make size (0, 1) in
    for i = 0 to n - 1 do
      gp.(i) <- (bit a i land bit b i, bit a i lxor bit b i)
    done;
    let orig = Array.copy gp in
    let stride = ref 1 in
    while !stride < size do
      let i = ref ((2 * !stride) - 1) in
      while !i < size do
        gp.(!i) <- op gp.(!i) gp.(!i - !stride);
        i := !i + (2 * !stride)
      done;
      stride := !stride * 2
    done;
    gp.(size - 1) <- (0, 1);
    let stride = ref (size / 2) in
    while !stride >= 1 do
      let i = ref ((2 * !stride) - 1) in
      while !i < size do
        let t = gp.(!i - !stride) in
        gp.(!i - !stride) <- gp.(!i);
        gp.(!i) <- op gp.(!i) t;
        i := !i + (2 * !stride)
      done;
      stride := !stride / 2
    done;
    let g0, p0 = orig.(n - 1) in
    let excl_g, _ = gp.(n - 1) in
    print_int (if g0 = 1 || (p0 = 1 && excl_g = 1) then 1 else 0);
    for i = n - 1 downto 0 do
      let carry, _ = gp.(i) in
      print_int (bit a i lxor bit b i lxor carry)
    done
  with End_of_file -> ()
