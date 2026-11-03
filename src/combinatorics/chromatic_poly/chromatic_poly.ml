module EdgeSet = Set.Make(struct
  type t = int * int
  let compare = compare
end)

let ipow b n =
  let r = ref 1L in
  for _ = 1 to n do
    r := Int64.mul !r b
  done;
  !r

let rec chrom n edges k =
  let s = ref EdgeSet.empty in
  List.iter (fun (u, v) ->
    let a = min u v in
    let b = max u v in
    if a <> b then s := EdgeSet.add (a, b) !s
  ) edges;
  let es = EdgeSet.elements !s in
  match es with
  | [] -> ipow k n
  | (u, v) :: rest ->
    let p1 = chrom n rest k in
    let mp = Array.make n (-1) in
    let nxt = ref 0 in
    for i = 0 to n - 1 do
      if i <> v then begin
        mp.(i) <- !nxt;
        incr nxt
      end
    done;
    let ne = ref [] in
    List.iter (fun (ea, eb) ->
      let a = if ea = v then mp.(u) else mp.(ea) in
      let b = if eb = v then mp.(u) else mp.(eb) in
      if a <> b then ne := (a, b) :: !ne
    ) rest;
    let p2 = chrom (n - 1) (List.rev !ne) k in
    Int64.sub p1 p2

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let m = Scanf.scanf " %d" (fun x -> x) in
    let k = Scanf.scanf " %Ld" (fun x -> x) in
    let edges = ref [] in
    for _ = 1 to m do
      let u = Scanf.scanf " %d" (fun x -> x) in
      let v = Scanf.scanf " %d" (fun x -> x) in
      edges := (u, v) :: !edges
    done;
    let ans = chrom n (List.rev !edges) k in
    Printf.printf "%Ld\n" ans
  with End_of_file -> ()
