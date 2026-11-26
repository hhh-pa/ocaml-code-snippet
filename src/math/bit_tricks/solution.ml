(* Integer bit tricks (exact arithmetic on values < 2^62). *)

let bt_popcount x =
  let c = ref 0 and v = ref x in
  while !v <> Int64.zero do
    if Int64.logand !v Int64.one <> Int64.zero then incr c;
    v := Int64.shift_right_logical !v 1
  done;
  !c

let bt_ctz x =
  if x = Int64.zero then 64
  else
    let c = ref 0 and v = ref x in
    while Int64.logand !v Int64.one = Int64.zero do
      c := !c + 1;
      v := Int64.shift_right_logical !v 1
    done;
    !c

let bt_reverse x w =
  let r = ref Int64.zero in
  for i = 0 to w - 1 do
    let b = Int64.logand (Int64.shift_right_logical x i) Int64.one in
    r := Int64.logor !r (Int64.shift_left b (w - 1 - i))
  done;
  !r

let bt_nextpow2 x =
  if x <= Int64.one then Int64.one
  else
    let p = ref Int64.one in
    while !p < x do
      p := Int64.shift_left !p 1
    done;
    !p

let bt_solve op a b =
  match op with
  | "POPCOUNT" -> Int64.of_int (bt_popcount a)
  | "PARITY" -> Int64.of_int (bt_popcount a land 1)
  | "ISPOW2" ->
      if a > Int64.zero && Int64.logand a (Int64.sub a Int64.one) = Int64.zero
      then Int64.one
      else Int64.zero
  | "CTZ" -> Int64.of_int (bt_ctz a)
  | "REVERSE" -> bt_reverse a b
  | _ -> bt_nextpow2 a

let read_all () =
  let buf = Buffer.create 256 in
  (try
     while true do
       Buffer.add_char buf (input_char stdin)
     done
   with End_of_file -> ());
  Buffer.contents buf

let is_space = function ' ' | '\t' | '\n' | '\r' -> true | _ -> false

let split_ws s =
  let toks = ref [] in
  let cur = Buffer.create 16 in
  let flush () =
    if Buffer.length cur > 0 then begin
      toks := Buffer.contents cur :: !toks;
      Buffer.clear cur
    end
  in
  String.iter
    (fun c -> if is_space c then flush () else Buffer.add_char cur c)
    s;
  flush ();
  List.rev !toks

let () =
  match split_ws (read_all ()) with
  | [] -> ()
  | toks ->
      let nums = Array.of_list toks in
      let q = int_of_string nums.(0) in
      let pos = ref 1 in
      let out = ref [] in
      for _ = 1 to q do
        if !pos < Array.length nums then begin
          let op = nums.(!pos) in
          incr pos;
          let a = Int64.of_string nums.(!pos) in
          incr pos;
          let b =
            if op = "REVERSE" then begin
              let v = int_of_string nums.(!pos) in
              incr pos;
              v
            end
            else 0
          in
          out := Int64.to_string (bt_solve op a b) :: !out
        end
      done;
      print_string (String.concat "\n" (List.rev !out))
