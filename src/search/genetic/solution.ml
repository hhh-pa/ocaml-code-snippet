(* Genetic algorithms for N-queens (AIMA 4.1.4). *)

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

let lcg_mod = 1 lsl 31

let draw rng upper =
  rng := ((1103515245 * !rng) + 12345) mod lcg_mod;
  !rng mod upper

let fitness s =
  let n = Array.length s in
  let bad = ref 0 in
  for i = 0 to n - 1 do
    for j = i + 1 to n - 1 do
      if s.(i) = s.(j) || abs (s.(i) - s.(j)) = j - i then incr bad
    done
  done;
  (n * (n - 1) / 2) - !bad

let select_parent pop fit total rng =
  if total = 0 then Array.copy pop.(draw rng (Array.length pop))
  else
    let w = draw rng total in
    let acc = ref 0 in
    let res = ref (Array.length pop - 1) in
    let found = ref false in
    for i = 0 to Array.length pop - 1 do
      acc := !acc + fit.(i);
      if (not !found) && w < !acc then begin
        res := i;
        found := true
      end
    done;
    Array.copy pop.(!res)

let make_child p1 p2 c mnum mden rng =
  Array.mapi
    (fun j _ ->
      let g = if j < c then p1.(j) else p2.(j) in
      if draw rng mden < mnum then 1 + draw rng (Array.length p1) else g)
    p1

let () =
  match split_ws (read_all ()) with
  | [] -> ()
  | toks ->
      let t = Array.of_list toks in
      let n = int_of_string t.(0) in
      let p = int_of_string t.(1) in
      let g = int_of_string t.(2) in
      let mnum = int_of_string t.(3) in
      let mden = int_of_string t.(4) in
      let rng = ref (int_of_string t.(5)) in
      let pop =
        Array.init p (fun i ->
            Array.init n (fun j -> Char.code t.(6 + i).[j] - Char.code '0'))
      in
      for _ = 1 to g do
        let fit = Array.map fitness pop in
        let total = Array.fold_left ( + ) 0 fit in
        let nxt = ref [] in
        for _ = 1 to p / 2 do
          let a = select_parent pop fit total rng in
          let b = select_parent pop fit total rng in
          let c = 1 + draw rng (n - 1) in
          nxt :=
            make_child b a c mnum mden rng
            :: make_child a b c mnum mden rng
            :: !nxt
        done;
        Array.blit (Array.of_list (List.rev !nxt)) 0 pop 0 p
      done;
      let bi = ref 0 in
      let bf = ref (fitness pop.(0)) in
      for i = 1 to p - 1 do
        let f = fitness pop.(i) in
        if f > !bf then begin
          bf := f;
          bi := i
        end
      done;
      let out = Buffer.create 256 in
      for i = 0 to p - 1 do
        Array.iter
          (fun gene -> Buffer.add_char out (Char.chr (Char.code '0' + gene)))
          pop.(i);
        Buffer.add_char out '\n'
      done;
      Buffer.add_string out "BEST ";
      Array.iter
        (fun gene -> Buffer.add_char out (Char.chr (Char.code '0' + gene)))
        pop.(!bi);
      Buffer.add_string out (Printf.sprintf " %d" !bf);
      print_string (Buffer.contents out)
