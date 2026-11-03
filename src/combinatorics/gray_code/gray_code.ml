let gray_to_bin g =
  let b = ref g in
  let g = ref (g lsr 1) in
  while !g > 0 do
    b := !b lxor !g;
    g := !g lsr 1
  done;
  !b

let fmt_gray g n =
  String.init n (fun i ->
    let bit = (g lsr (n - 1 - i)) land 1 in
    if bit = 1 then '1' else '0'
  )

let () =
  try
    let mode = Scanf.scanf " %s" (fun s -> s) in
    if mode = "G" then begin
      let n = Scanf.scanf " %d" (fun x -> x) in
      let total = 1 lsl n in
      for i = 0 to total - 1 do
        Printf.printf "%s\n" (fmt_gray (i lxor (i lsr 1)) n)
      done
    end else if mode = "R" then begin
      let g = Scanf.scanf " %s" (fun s -> s) in
      let v = ref 0 in
      for i = 0 to String.length g - 1 do
        v := !v * 2 + (Char.code g.[i] - Char.code '0')
      done;
      Printf.printf "%d\n" (gray_to_bin !v)
    end else begin
      let n = Scanf.scanf " %d" (fun x -> x) in
      let r = Scanf.scanf " %d" (fun x -> x) in
      Printf.printf "%s\n" (fmt_gray (r lxor (r lsr 1)) n)
    end
  with End_of_file -> ()
