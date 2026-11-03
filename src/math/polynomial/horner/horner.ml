let mod_val = 1_000_000_007L

let mod_norm x =
  let r = Int64.rem x mod_val in
  if r < 0L then Int64.add r mod_val else r

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    if n < 0 then exit 0;
    let c = Array.init (n + 1) (fun _ -> mod_norm (Scanf.scanf " %Ld" (fun x -> x))) in
    let q = Scanf.scanf " %d" (fun x -> x) in
    for query_idx = 0 to q - 1 do
      let x = mod_norm (Scanf.scanf " %Ld" (fun v -> v)) in
      let res = ref c.(n) in
      for i = n - 1 downto 0 do
        res := Int64.rem (Int64.add (Int64.mul !res x) c.(i)) mod_val
      done;
      if query_idx > 0 then print_string " ";
      Printf.printf "%Ld" !res
    done;
    print_newline ()
  with End_of_file -> ()
