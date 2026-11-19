let () =
  try
    let a = Scanf.scanf " %s" (fun x -> x) in
    let b = Scanf.scanf " %s" (fun x -> x) in
    let n = max (String.length a) (String.length b) in
    let pad s = String.make (n - String.length s) '0' ^ s in
    let a = pad a and b = pad b in
    let out = Buffer.create (n + 1) in
    let res = Array.make (n + 1) '0' in
    let carry = ref 0 in
    let base = ref 0 in
    while !base < n do
      let w = min 4 (n - !base) in
      let solve cin =
        let s = Array.make w '0' in
        let c = ref cin in
        for j = 0 to w - 1 do
          let x = Char.code a.[n - 1 - (!base + j)] - Char.code '0' in
          let y = Char.code b.[n - 1 - (!base + j)] - Char.code '0' in
          let bit = x + y + !c in
          s.(w - 1 - j) <- Char.chr (Char.code '0' + (bit land 1));
          c := bit lsr 1
        done;
        (s, !c)
      in
      let s0, c0 = solve 0 and s1, c1 = solve 1 in
      let sel = if !carry = 0 then s0 else s1 in
      for j = 0 to w - 1 do
        res.(n - !base - w + 1 + j) <- sel.(j)
      done;
      carry := if !carry = 0 then c0 else c1;
      base := !base + 4
    done;
    res.(0) <- Char.chr (Char.code '0' + !carry);
    Array.iter (Buffer.add_char out) res;
    print_string (Buffer.contents out)
  with End_of_file -> ()
