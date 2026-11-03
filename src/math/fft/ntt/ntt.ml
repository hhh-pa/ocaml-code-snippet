let mod_val = 998244353L
let g_val = 3L

let modpow b e =
  let rec loop cur_b cur_e r =
    if cur_e = 0L then r
    else
      let new_r = if Int64.rem cur_e 2L = 1L then Int64.rem (Int64.mul r cur_b) mod_val else r in
      let new_b = Int64.rem (Int64.mul cur_b cur_b) mod_val in
      loop new_b (Int64.div cur_e 2L) new_r
  in
  loop (Int64.rem b mod_val) e 1L

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let a = Array.init n (fun _ ->
      let v = Scanf.scanf " %Ld" (fun x -> x) in
      let rem = Int64.rem v mod_val in
      if rem < 0L then Int64.add rem mod_val else rem
    ) in

    let j = ref 0 in
    for i = 1 to n - 1 do
      let bit = ref (n lsr 1) in
      while (!j land !bit) <> 0 do
        j := !j lxor !bit;
        bit := !bit lsr 1
      done;
      j := !j lxor !bit;
      if i < !j then begin
        let t = a.(i) in
        a.(i) <- a.(!j);
        a.(!j) <- t
      end
    done;

    let len = ref 2 in
    while !len <= n do
      let half = !len lsr 1 in
      let wlen = modpow g_val (Int64.div (Int64.sub mod_val 1L) (Int64.of_int !len)) in
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

    for i = 0 to n - 1 do
      if i > 0 then print_string " ";
      Printf.printf "%Ld" a.(i)
    done;
    print_newline ()
  with End_of_file -> ()
