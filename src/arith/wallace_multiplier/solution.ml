let () =
  try
    let a = Scanf.scanf " %s" (fun x -> x) in
    let b = Scanf.scanf " %s" (fun x -> x) in
    let n = max (String.length a) (String.length b) in
    let pad s = String.make (n - String.length s) '0' ^ s in
    let a = pad a and b = pad b in
    let width = (2 * n) + 2 in
    let cols = Array.make width [] in
    for j = 0 to n - 1 do
      if b.[n - 1 - j] = '1' then
        for i = 0 to n - 1 do
          cols.(i + j) <-
            (Char.code a.[n - 1 - i] - Char.code '0') :: cols.(i + j)
        done
    done;
    let deepest () =
      Array.fold_left (fun m c -> max m (List.length c)) 0 cols
    in
    while deepest () > 2 do
      let nxt = Array.make width [] in
      for c = 0 to width - 2 do
        let bits = Array.of_list cols.(c) in
        let triples = Array.length bits / 3 in
        for t = 0 to triples - 1 do
          let x = bits.(3 * t)
          and y = bits.((3 * t) + 1)
          and z = bits.((3 * t) + 2) in
          nxt.(c) <- (x lxor y lxor z) :: nxt.(c);
          nxt.(c + 1) <- ((x + y + z) lsr 1) :: nxt.(c + 1)
        done;
        for k = 3 * triples to Array.length bits - 1 do
          nxt.(c) <- bits.(k) :: nxt.(c)
        done
      done;
      nxt.(width - 1) <- cols.(width - 1) @ nxt.(width - 1);
      Array.blit nxt 0 cols 0 width
    done;
    let carry = ref 0 in
    let out = Buffer.create (2 * n) in
    for c = 0 to (2 * n) - 1 do
      let bit =
        !carry
        + (match cols.(c) with h :: _ -> h | [] -> 0)
        + match cols.(c) with _ :: h :: _ -> h | _ -> 0
      in
      Buffer.add_char out (Char.chr (Char.code '0' + (bit land 1)));
      carry := bit lsr 1
    done;
    let s = Buffer.contents out in
    for i = String.length s - 1 downto 0 do
      print_char s.[i]
    done
  with End_of_file -> ()
