let mod_val = 998244353L
let g_val = 3L

let power base exp =
  let rec loop b e acc =
    if e = 0L then acc
    else
      let new_acc = if Int64.rem e 2L = 1L then Int64.rem (Int64.mul acc b) mod_val else acc in
      let new_b = Int64.rem (Int64.mul b b) mod_val in
      loop new_b (Int64.div e 2L) new_acc
  in
  loop (Int64.rem base mod_val) exp 1L

let mod_inverse n =
  power n (Int64.sub mod_val 2L)

let ntt a invert =
  let n = Array.length a in
  let j = ref 0 in
  for i = 1 to n - 1 do
    let bit = ref (n lsr 1) in
    while (!j land !bit) <> 0 do
      j := !j lxor !bit;
      bit := !bit lsr 1
    done;
    j := !j lxor !bit;
    if i < !j then begin
      let tmp = a.(i) in
      a.(i) <- a.(!j);
      a.(!j) <- tmp
    end
  done;

  let len = ref 2 in
  while !len <= n do
    let half = !len lsr 1 in
    let exp = Int64.div (Int64.sub mod_val 1L) (Int64.of_int !len) in
    let wlen_init = power g_val exp in
    let wlen = if invert then mod_inverse wlen_init else wlen_init in
    let i = ref 0 in
    while !i < n do
      let w = ref 1L in
      for j_sub = 0 to half - 1 do
        let idx1 = !i + j_sub in
        let idx2 = !i + j_sub + half in
        let u = a.(idx1) in
        let v = Int64.rem (Int64.mul a.(idx2) !w) mod_val in
        let add_uv = Int64.add u v in
        a.(idx1) <- (if add_uv < mod_val then add_uv else Int64.sub add_uv mod_val);
        let sub_uv = Int64.sub u v in
        a.(idx2) <- (if sub_uv >= 0L then sub_uv else Int64.add sub_uv mod_val);
        w := Int64.rem (Int64.mul !w wlen) mod_val
      done;
      i := !i + !len
    done;
    len := !len lsl 1
  done;

  if invert then begin
    let n_inv = mod_inverse (Int64.of_int n) in
    for i = 0 to n - 1 do
      a.(i) <- Int64.rem (Int64.mul a.(i) n_inv) mod_val
    done
  end

let () =
  try
    let deg_a = Scanf.scanf " %d" (fun x -> x) in
    let deg_b = Scanf.scanf " %d" (fun x -> x) in
    let raw_a = Array.init (deg_a + 1) (fun _ -> Scanf.scanf " %Ld" (fun x -> x)) in
    let raw_b = Array.init (deg_b + 1) (fun _ -> Scanf.scanf " %Ld" (fun x -> x)) in

    let n = ref 1 in
    while !n < deg_a + deg_b + 1 do
      n := !n lsl 1
    done;

    let a = Array.make !n 0L in
    let b = Array.make !n 0L in
    Array.blit raw_a 0 a 0 (deg_a + 1);
    Array.blit raw_b 0 b 0 (deg_b + 1);

    ntt a false;
    ntt b false;

    for i = 0 to !n - 1 do
      a.(i) <- Int64.rem (Int64.mul a.(i) b.(i)) mod_val
    done;

    ntt a true;

    let final_deg = deg_a + deg_b in
    Printf.printf "%d\n" final_deg;
    for i = 0 to final_deg do
      if i > 0 then print_string " ";
      Printf.printf "%Ld" a.(i)
    done;
    print_newline ()
  with End_of_file -> ()
