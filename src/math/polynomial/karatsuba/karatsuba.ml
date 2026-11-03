let mod_val = 1_000_000_007L

let mod_norm x =
  let r = Int64.rem x mod_val in
  if r < 0L then Int64.add r mod_val else r

let rec karatsuba a b =
  let n = Array.length a in
  let m = Array.length b in
  if n = 0 || m = 0 then [||]
  else if min n m <= 32 then begin
    let c = Array.make (n + m - 1) 0L in
    for i = 0 to n - 1 do
      let ai = a.(i) in
      for j = 0 to m - 1 do
        c.(i + j) <- mod_norm (Int64.add c.(i + j) (Int64.mul ai b.(j)))
      done
    done;
    c
  end else begin
    let k = max n m / 2 in
    let len_a0 = min k n in
    let len_a1 = max 0 (n - k) in
    let a0 = Array.sub a 0 len_a0 in
    let a1 = if len_a1 > 0 then Array.sub a k len_a1 else [||] in

    let len_b0 = min k m in
    let len_b1 = max 0 (m - k) in
    let b0 = Array.sub b 0 len_b0 in
    let b1 = if len_b1 > 0 then Array.sub b k len_b1 else [||] in

    let z0 = karatsuba a0 b0 in
    let z2 = karatsuba a1 b1 in

    let asz = max len_a0 len_a1 in
    let bsz = max len_b0 len_b1 in
    let asum = Array.make asz 0L in
    let bsum = Array.make bsz 0L in

    for i = 0 to len_a0 - 1 do
      asum.(i) <- mod_norm (Int64.add asum.(i) a0.(i))
    done;
    for i = 0 to len_a1 - 1 do
      asum.(i) <- mod_norm (Int64.add asum.(i) a1.(i))
    done;

    for i = 0 to len_b0 - 1 do
      bsum.(i) <- mod_norm (Int64.add bsum.(i) b0.(i))
    done;
    for i = 0 to len_b1 - 1 do
      bsum.(i) <- mod_norm (Int64.add bsum.(i) b1.(i))
    done;

    let z1_raw = karatsuba asum bsum in
    let target_len = max (Array.length z1_raw) (max (Array.length z0) (Array.length z2)) in
    let z1 = Array.make target_len 0L in
    Array.blit z1_raw 0 z1 0 (Array.length z1_raw);

    for i = 0 to Array.length z0 - 1 do
      z1.(i) <- mod_norm (Int64.sub z1.(i) z0.(i))
    done;
    for i = 0 to Array.length z2 - 1 do
      z1.(i) <- mod_norm (Int64.sub z1.(i) z2.(i))
    done;

    let res = Array.make (n + m - 1) 0L in
    for i = 0 to Array.length z0 - 1 do
      if i < Array.length res then
        res.(i) <- mod_norm (Int64.add res.(i) z0.(i))
    done;
    for i = 0 to Array.length z1 - 1 do
      if i + k < Array.length res then
        res.(i + k) <- mod_norm (Int64.add res.(i + k) z1.(i))
    done;
    for i = 0 to Array.length z2 - 1 do
      if i + 2 * k < Array.length res then
        res.(i + 2 * k) <- mod_norm (Int64.add res.(i + 2 * k) z2.(i))
    done;
    res
  end

let () =
  try
    let da = Scanf.scanf " %d" (fun x -> x) in
    let db = Scanf.scanf " %d" (fun x -> x) in
    let a = Array.init (da + 1) (fun _ -> mod_norm (Scanf.scanf " %Ld" (fun x -> x))) in
    let b = Array.init (db + 1) (fun _ -> mod_norm (Scanf.scanf " %Ld" (fun x -> x))) in

    let c = karatsuba a b in
    let dc = da + db in
    Printf.printf "%d\n" dc;
    for i = 0 to dc do
      if i > 0 then print_string " ";
      let v = if i < Array.length c then c.(i) else 0L in
      Printf.printf "%Ld" v
    done;
    print_newline ()
  with End_of_file -> ()
