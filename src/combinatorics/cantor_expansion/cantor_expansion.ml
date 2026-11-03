let fact n =
  let f = ref 1L in
  for i = 2 to n do
    f := Int64.mul !f (Int64.of_int i)
  done;
  !f

let dval c =
  if c >= '0' && c <= '9' then Char.code c - Char.code '0'
  else Char.code (Char.uppercase_ascii c) - Char.code 'A' + 10

let dchar v =
  if v < 10 then Char.chr (Char.code '0' + v)
  else Char.chr (Char.code 'A' + v - 10)

let stripz s =
  let len = String.length s in
  let rec find i =
    if i = len then -1
    else if s.[i] <> '0' then i
    else find (i + 1)
  in
  match find 0 with
  | -1 -> "0"
  | p -> String.sub s p (len - p)

let add_base x y b =
  let i = ref (String.length x - 1) in
  let j = ref (String.length y - 1) in
  let carry = ref 0 in
  let res = ref [] in
  while !i >= 0 || !j >= 0 || !carry > 0 do
    let s = ref !carry in
    if !i >= 0 then begin s := !s + dval x.[!i]; decr i end;
    if !j >= 0 then begin s := !s + dval y.[!j]; decr j end;
    res := dchar (!s mod b) :: !res;
    carry := !s / b
  done;
  let len = List.length !res in
  let buf = Buffer.create len in
  List.iter (Buffer.add_char buf) !res;
  stripz (Buffer.contents buf)

let mul_base x y b =
  let x = stripz x in
  let y = stripz y in
  if x = "0" || y = "0" then "0"
  else begin
    let len_x = String.length x in
    let len_y = String.length y in
    let a = Array.init len_x (fun idx -> dval x.[len_x - 1 - idx]) in
    let cc = Array.init len_y (fun idx -> dval y.[len_y - 1 - idx]) in
    let r = Array.make (len_x + len_y) 0 in
    for i = 0 to len_x - 1 do
      for j = 0 to len_y - 1 do
        r.(i + j) <- r.(i + j) + a.(i) * cc.(j)
      done
    done;
    let carry = ref 0 in
    for i = 0 to len_x + len_y - 1 do
      let t = r.(i) + !carry in
      r.(i) <- t mod b;
      carry := t / b
    done;
    let extra = ref [] in
    while !carry > 0 do
      extra := (!carry mod b) :: !extra;
      carry := !carry / b
    done;
    let full = Array.to_list r @ List.rev !extra in
    let rev_full = List.rev full in
    let buf = Buffer.create (List.length rev_full) in
    List.iter (fun v -> Buffer.add_char buf (dchar v)) rev_full;
    stripz (Buffer.contents buf)
  end

let rec remove_at k = function
  | [] -> failwith "empty"
  | x :: xs ->
    if k = 0 then (x, xs)
    else
      let (v, rest) = remove_at (k - 1) xs in
      (v, x :: rest)

let () =
  try
    let mode = Scanf.scanf " %s" (fun s -> s) in
    if mode = "E" then begin
      let n = Scanf.scanf " %d" (fun x -> x) in
      let p = Array.init n (fun _ -> Scanf.scanf " %d" (fun x -> x)) in
      let l_list = ref [] in
      let rank = ref 0L in
      for i = 0 to n - 2 do
        let c = ref 0 in
        for j = i + 1 to n - 1 do
          if p.(j) < p.(i) then incr c
        done;
        l_list := !c :: !l_list;
        rank := Int64.add !rank (Int64.mul (Int64.of_int !c) (fact (n - 1 - i)))
      done;
      let l_list = List.rev !l_list in
      List.iteri (fun idx v ->
        if idx > 0 then print_char ' ';
        print_int v
      ) l_list;
      print_newline ();
      Printf.printf "%Ld\n" !rank
    end else if mode = "D" then begin
      let n = Scanf.scanf " %d" (fun x -> x) in
      let l_arr = Array.make n 0 in
      for i = 0 to n - 2 do
        l_arr.(i) <- Scanf.scanf " %d" (fun x -> x)
      done;
      l_arr.(n - 1) <- 0;
      let avail = ref (List.init n (fun i -> i)) in
      for i = 0 to n - 1 do
        if i > 0 then print_char ' ';
        let (v, rest) = remove_at l_arr.(i) !avail in
        avail := rest;
        print_int v
      done;
      print_newline ()
    end else if mode = "A" then begin
      let b = Scanf.scanf " %d" (fun x -> x) in
      let x = Scanf.scanf " %s" (fun s -> s) in
      let y = Scanf.scanf " %s" (fun s -> s) in
      Printf.printf "%s\n" (add_base x y b)
    end else begin
      let b = Scanf.scanf " %d" (fun x -> x) in
      let x = Scanf.scanf " %s" (fun s -> s) in
      let y = Scanf.scanf " %s" (fun s -> s) in
      Printf.printf "%s\n" (mul_base x y b)
    end
  with End_of_file -> ()
