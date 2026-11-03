let mod_val = 1000000007L

let add_mod a b =
  let s = Int64.rem (Int64.add a b) mod_val in
  if s < 0L then Int64.add s mod_val else s

let sub_mod a b =
  let s = Int64.rem (Int64.sub a b) mod_val in
  if s < 0L then Int64.add s mod_val else s

let mul_mod a b =
  let s = Int64.rem (Int64.mul (Int64.rem a mod_val) (Int64.rem b mod_val)) mod_val in
  if s < 0L then Int64.add s mod_val else s

type fenwick = { n: int; tree: int64 array }

let make_fenwick n = { n; tree = Array.make (n + 1) 0L }

let fenwick_add f i delta =
  let idx = ref i in
  while !idx <= f.n do
    f.tree.(!idx) <- add_mod f.tree.(!idx) delta;
    idx := !idx + (!idx land (- !idx))
  done

let fenwick_query f i =
  let s = ref 0L in
  let idx = ref i in
  while !idx > 0 do
    s := add_mod !s f.tree.(!idx);
    idx := !idx - (!idx land (- !idx))
  done;
  !s

let fenwick_query_range f l r =
  if l > r then 0L
  else sub_mod (fenwick_query f r) (fenwick_query f (l - 1))

let lower_bound arr target =
  let l = ref 0 in
  let r = ref (Array.length arr) in
  while !l < !r do
    let mid = (!l + !r) / 2 in
    if arr.(mid) < target then l := mid + 1
    else r := mid
  done;
  !l

type city = { x: int64; p: int64; rank: int }

let solve () =
  let n = Scanf.scanf " %d" (fun x -> x) in
  let xs = Array.init n (fun _ -> Scanf.scanf " %Ld" (fun x -> x)) in
  let ps = Array.init n (fun _ -> Scanf.scanf " %Ld" (fun x -> x)) in
  let coords = Array.copy xs in
  Array.sort compare coords;
  let len = ref 1 in
  for i = 1 to n - 1 do
    if coords.(i) <> coords.(!len - 1) then begin
      coords.(!len) <- coords.(i);
      incr len
    end
  done;
  let unique_coords = Array.sub coords 0 !len in
  let m = Array.length unique_coords in
  let cities = Array.init n (fun i ->
    let r = lower_bound unique_coords xs.(i) + 1 in
    { x = xs.(i); p = ps.(i); rank = r }
  ) in
  Array.sort (fun a b -> compare a.p b.p) cities;
  let count_bit = make_fenwick m in
  let sum_bit = make_fenwick m in
  let total_cost = ref 0L in
  for i = 0 to n - 1 do
    let x = cities.(i).x in
    let p_mod = Int64.rem cities.(i).p mod_val in
    let x_mod = Int64.rem x mod_val in
    let r = cities.(i).rank in

    let count_left = fenwick_query count_bit (r - 1) in
    let sum_left = fenwick_query sum_bit (r - 1) in

    let count_right = fenwick_query_range count_bit (r + 1) m in
    let sum_right = fenwick_query_range sum_bit (r + 1) m in

    let left_contrib = sub_mod (mul_mod count_left x_mod) sum_left in
    let right_contrib = sub_mod sum_right (mul_mod count_right x_mod) in

    let cost = mul_mod (add_mod left_contrib right_contrib) p_mod in
    total_cost := add_mod !total_cost cost;

    fenwick_add count_bit r 1L;
    fenwick_add sum_bit r x_mod
  done;
  Printf.printf "%Ld\n" !total_cost

let () =
  try
    let t = Scanf.scanf " %d" (fun x -> x) in
    for _ = 1 to t do
      solve ()
    done
  with End_of_file -> ()
