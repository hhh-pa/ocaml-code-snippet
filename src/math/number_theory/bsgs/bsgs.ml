let rec ext_gcd a b =
  if b = 0L then (a, 1L, 0L)
  else
    let q = Int64.div a b in
    let r = Int64.rem a b in
    let (g, x1, y1) = ext_gcd b r in
    (g, y1, Int64.sub x1 (Int64.mul q y1))

let mod_inv a m =
  let (g, x, _) = ext_gcd a m in
  if g = 1L then
    let res = Int64.rem x m in
    if res < 0L then Int64.add res m else res
  else 1L

let mod_pow base exp m =
  let rec loop b e acc =
    if e = 0L then acc
    else
      let new_acc = if Int64.rem e 2L = 1L then Int64.rem (Int64.mul acc b) m else acc in
      let new_b = Int64.rem (Int64.mul b b) m in
      loop new_b (Int64.div e 2L) new_acc
  in
  loop (Int64.rem base m) exp (Int64.rem 1L m)

let () =
  try
    let g = Scanf.scanf " %Ld" (fun x -> x) in
    let h = Scanf.scanf " %Ld" (fun x -> x) in
    let p = Scanf.scanf " %Ld" (fun x -> x) in

    let norm x =
      let r = Int64.rem x p in
      if r < 0L then Int64.add r p else r
    in
    let g = norm g in
    let h = norm h in

    if h = 1L then begin
      Printf.printf "0\n";
      exit 0
    end;

    let fp = Int64.to_float p in
    let m = Int64.of_float (ceil (sqrt fp)) in
    let table = Hashtbl.create (Int64.to_int (min 1000000L m)) in

    let cur = ref 1L in
    let j = ref 0L in
    while !j < m do
      if not (Hashtbl.mem table !cur) then
        Hashtbl.add table !cur !j;
      cur := Int64.rem (Int64.mul !cur g) p;
      j := Int64.add !j 1L
    done;

    let gm = mod_pow g m p in
    let factor = mod_inv gm p in

    let gamma = ref h in
    let found = ref false in
    let i = ref 0L in
    while !i < m && not !found do
      match Hashtbl.find_opt table !gamma with
      | Some tbl_val ->
        let ans = Int64.add (Int64.mul !i m) tbl_val in
        if mod_pow g ans p = h then begin
          Printf.printf "%Ld\n" ans;
          found := true
        end else begin
          gamma := Int64.rem (Int64.mul !gamma factor) p;
          i := Int64.add !i 1L
        end
      | None ->
        gamma := Int64.rem (Int64.mul !gamma factor) p;
        i := Int64.add !i 1L
    done;

    if not !found then
      Printf.printf "NONE\n"
  with End_of_file -> ()
