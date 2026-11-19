(* NTRUEncrypt (toy N=7, p=3, q=41). *)

let polymul a b n md =
  let c = Array.make n 0L in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      c.((i + j) mod n) <-
        Int64.rem (Int64.add c.((i + j) mod n) (Int64.mul a.(i) b.(j))) md
    done
  done;
  Array.map (fun v -> Int64.rem (Int64.add (Int64.rem v md) md) md) c

let center v md =
  let r = Int64.rem (Int64.add (Int64.rem v md) md) md in
  if r > Int64.div md 2L then Int64.sub r md else r

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
  | op :: ns :: ps :: qs :: rest ->
      let n = int_of_string ns
      and p = Int64.of_string ps
      and q = Int64.of_string qs in
      let vals = Array.of_list (List.map Int64.of_string rest) in
      let a = Array.sub vals 0 n
      and b = Array.sub vals n n
      and c = Array.sub vals (2 * n) n in
      let out =
        if op = "ENC" then
          let t = polymul b a n q in
          Array.init n (fun i -> center (Int64.add t.(i) c.(i)) q)
        else
          let aq =
            Array.map (fun v -> Int64.rem (Int64.add (Int64.rem v q) q) q) a
          in
          let eq =
            Array.map (fun v -> Int64.rem (Int64.add (Int64.rem v q) q) q) c
          in
          let t = polymul aq eq n q in
          let ap =
            Array.map
              (fun v -> Int64.rem (Int64.add (Int64.rem (center v q) p) p) p)
              t
          in
          polymul b ap n p
      in
      print_string
        (String.concat " "
           (List.map (Printf.sprintf "%Ld") (Array.to_list out)))
  | _ -> ()
