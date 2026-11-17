let base = 256
let modp = 1000000007

let rabin_karp text pat =
  let n = String.length text and m = String.length pat in
  if m = 0 then true
  else if m > n then false
  else begin
    let pw = ref 1 in
    for _ = 1 to m - 1 do
      pw := !pw * base mod modp
    done;
    let h s off =
      let acc = ref 0 in
      for i = 0 to m - 1 do
        acc := ((!acc * base) + Char.code s.[off + i]) mod modp
      done;
      !acc
    in
    let ph = h pat 0 in
    let th = ref (h text 0) in
    let found = ref false in
    let i = ref 0 in
    while !i + m <= n && not !found do
      if !th = ph && String.sub text !i m = pat then found := true
      else begin
        if !i + m < n then begin
          th :=
            (((!th - (Char.code text.[!i] * !pw mod modp)) mod modp) + modp)
            mod modp;
          th := ((!th * base) + Char.code text.[!i + m]) mod modp
        end;
        incr i
      end
    done;
    !found
  end

let () =
  try
    let t = int_of_string (input_line stdin) in
    for i = 0 to t - 1 do
      let text = try input_line stdin with End_of_file -> "" in
      let pat = try input_line stdin with End_of_file -> "" in
      if i > 0 then print_newline ();
      print_string (if rabin_karp text pat then "YES" else "NO")
    done
  with
  | End_of_file -> ()
  | Failure _ -> ()
