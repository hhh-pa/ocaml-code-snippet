(* Pearson + Spearman correlation (Think Stats 2e ch 7). *)

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

let pearson xs ys =
  let n = List.length xs in
  let sx = List.fold_left ( +. ) 0.0 xs in
  let sy = List.fold_left ( +. ) 0.0 ys in
  let mx = sx /. float_of_int n in
  let my = sy /. float_of_int n in
  let c, vx, vy =
    List.fold_left2
      (fun (c, vx, vy) x y ->
        let dx = x -. mx in
        let dy = y -. my in
        (c +. (dx *. dy), vx +. (dx *. dx), vy +. (dy *. dy)))
      (0.0, 0.0, 0.0) xs ys
  in
  if vx = 0.0 || vy = 0.0 then 0.0 else c /. sqrt (vx *. vy)

let midranks xs =
  let n = List.length xs in
  let arr = Array.of_list xs in
  let order = Array.init n (fun i -> i) in
  Array.sort (fun a b -> Float.compare arr.(a) arr.(b)) order;
  let ranks = Array.make n 0.0 in
  let i = ref 0 in
  while !i < n do
    let j = ref !i in
    while !j + 1 < n && arr.(order.(!j + 1)) = arr.(order.(!i)) do
      incr j
    done;
    let r = float_of_int (!i + !j + 2) /. 2.0 in
    for k = !i to !j do
      ranks.(order.(k)) <- r
    done;
    i := !j + 1
  done;
  Array.to_list ranks

let () =
  match split_ws (read_all ()) with
  | [] -> ()
  | toks ->
      let arr = Array.of_list toks in
      let n = int_of_string arr.(0) in
      let xs =
        List.init n (fun i -> float_of_int (int_of_string arr.(1 + (2 * i))))
      in
      let ys =
        List.init n (fun i -> float_of_int (int_of_string arr.(2 + (2 * i))))
      in
      let r = pearson xs ys in
      let rho = pearson (midranks xs) (midranks ys) in
      Printf.printf "PEARSON %.6f\nSPEARMAN %.6f" r rho
