let strip0 s =
  let len = String.length s in
  let i = ref 0 in
  while !i < len && s.[!i] = '0' do
    incr i
  done;
  if !i = len then "0"
  else String.sub s !i (len - !i)

let cmpn a b =
  let la = String.length a in
  let lb = String.length b in
  if la <> lb then compare la lb
  else String.compare a b

let subn a b =
  let la = String.length a in
  let lb = String.length b in
  let res = Buffer.create la in
  let carry = ref 0 in
  let i = ref (la - 1) in
  let j = ref (lb - 1) in
  while !i >= 0 do
    let da = Char.code a.[!i] - Char.code '0' in
    let db = if !j >= 0 then Char.code b.[!j] - Char.code '0' else 0 in
    let d = da - !carry - db in
    if d < 0 then begin
      carry := 1;
      Buffer.add_char res (Char.chr (Char.code '0' + d + 10))
    end else begin
      carry := 0;
      Buffer.add_char res (Char.chr (Char.code '0' + d))
    end;
    decr i;
    decr j
  done;
  let str = Buffer.contents res in
  let len_str = String.length str in
  let rev_bytes = Bytes.create len_str in
  for k = 0 to len_str - 1 do
    Bytes.set rev_bytes k str.[len_str - 1 - k]
  done;
  strip0 (Bytes.to_string rev_bytes)

let () =
  try
    let a_str = Scanf.scanf " %s" (fun s -> s) in
    let b_str = Scanf.scanf " %s" (fun s -> s) in
    let a = strip0 a_str in
    let b = strip0 b_str in
    if cmpn a b < 0 then begin
      Printf.printf "0\n%s\n" a
    end else begin
      let q = Buffer.create (String.length a) in
      let cur = ref "0" in
      for k = 0 to String.length a - 1 do
        let next_cur = if !cur = "0" then String.make 1 a.[k] else !cur ^ String.make 1 a.[k] in
        cur := strip0 next_cur;
        let cnt = ref 0 in
        while cmpn !cur b >= 0 do
          cur := subn !cur b;
          incr cnt
        done;
        Buffer.add_char q (Char.chr (Char.code '0' + !cnt))
      done;
      Printf.printf "%s\n%s\n" (strip0 (Buffer.contents q)) !cur
    end
  with End_of_file -> ()
