let clean_line s =
  let len = String.length s in
  if len > 0 && s.[len - 1] = '\r' then String.sub s 0 (len - 1) else s

let char_at s d =
  if d < String.length s then Char.code s.[d] else -1

let rec sort3str a lo hi d =
  if hi > lo then begin
    let v = char_at a.(lo) d in
    let lt = ref lo in
    let gt = ref hi in
    let i = ref (lo + 1) in
    while !i <= !gt do
      let t = char_at a.(!i) d in
      if t < v then begin
        let tmp = a.(!lt) in
        a.(!lt) <- a.(!i);
        a.(!i) <- tmp;
        incr lt;
        incr i
      end else if t > v then begin
        let tmp = a.(!i) in
        a.(!i) <- a.(!gt);
        a.(!gt) <- tmp;
        decr gt
      end else
        incr i
    done;
    sort3str a lo (!lt - 1) d;
    if v >= 0 then sort3str a !lt !gt (d + 1);
    sort3str a (!gt + 1) hi d
  end

let () =
  try
    let line1 = input_line stdin in
    let n = int_of_string (String.trim line1) in
    let a = Array.init n (fun _ -> clean_line (input_line stdin)) in
    if n > 0 then sort3str a 0 (n - 1) 0;
    print_string "[";
    for i = 0 to n - 1 do
      if i > 0 then print_string ",";
      print_string a.(i)
    done;
    print_string "]"
  with End_of_file -> ()
