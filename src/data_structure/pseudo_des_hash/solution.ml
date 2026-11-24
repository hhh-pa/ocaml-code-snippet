(* Pseudo-DES hashing (NR 7.2). *)

let c1 = [| 0xbaa96887; 0x1e17d32c; 0x03bcdc3c; 0x0f33d1b2 |]
let c2 = [| 0x4b0f3b58; 0xe874f0c3; 0x6955c5a6; 0x55a7ca46 |]
let m32 = 0xFFFFFFFF

let psdes lword rword =
  let lw = ref lword and rw = ref rword in
  for i = 0 to 1 do
    let iswap = !rw in
    let ia = ref (iswap lxor c1.(i)) in
    let itmpl = !ia land 0xffff and itmph = !ia lsr 16 in
    let ib = (itmpl * itmpl + ((lnot (itmph * itmph)) land m32)) land m32 in
    ia := (ib lsr 16) lor ((ib land 0xffff) lsl 16);
    rw := !lw lxor (((!ia lxor c2.(i)) + itmpl * itmph) land m32);
    lw := iswap
  done;
  (!lw, !rw)

let hashall arr =
  let m = Array.length arr in
  if m >= 2 then (
    let n = ref (m - 1) in
    n := !n lor (!n lsr 1);
    n := !n lor (!n lsr 2);
    n := !n lor (!n lsr 4);
    n := !n lor (!n lsr 8);
    n := !n lor (!n lsr 16);
    n := !n + 1;
    let n = !n in
    let nb = ref n and nb2 = ref (n lsr 1) in
    while !nb > 1 do
      let jb = ref 0 in
      while !jb < n - !nb + 1 do
        for j = 0 to !nb2 - 1 do
          if !jb + j + !nb2 < m then (
            let x, y = psdes arr.(!jb + j) arr.(!jb + j + !nb2) in
            arr.(!jb + j) <- x;
            arr.(!jb + j + !nb2) <- y)
        done;
        jb := !jb + !nb
      done;
      nb := !nb2;
      nb2 := !nb2 lsr 1
    done;
    nb2 := n lsr 1;
    if m <> n then
      for j = !nb2 to m - 1 do
        let x, y = psdes arr.(j) arr.(j - !nb2) in
        arr.(j) <- x;
        arr.(j - !nb2) <- y
      done)

let () =
  let toks =
    String.split_on_char ' '
      (String.concat " "
         (let rec rd acc =
            try rd (input_line stdin :: acc) with End_of_file -> List.rev acc
          in
          rd []))
  in
  let toks = Array.of_list (List.filter (fun s -> s <> "") toks) in
  if Array.length toks = 0 then exit 0;
  let op = toks.(0) in
  if op = "PSDES" then (
    let l, r = psdes (int_of_string toks.(1)) (int_of_string toks.(2)) in
    Printf.printf "%d %d\n" l r)
  else if op = "HASHALL" then (
    let m = int_of_string toks.(1) in
    let arr = Array.init m (fun i -> int_of_string toks.(2 + i)) in
    hashall arr;
    print_endline (String.concat " " (Array.to_list (Array.map string_of_int arr))))
