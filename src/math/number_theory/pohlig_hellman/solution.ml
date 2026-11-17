let modpow b e m =
  let r = ref (Int64.rem 1L m) in
  let b = ref (Int64.rem b m) in
  let e = ref e in
  while !e > 0L do
    if Int64.logand !e 1L = 1L then r := Int64.rem (Int64.mul !r !b) m;
    b := Int64.rem (Int64.mul !b !b) m;
    e := Int64.shift_right !e 1
  done;
  !r

let rec egcd a b =
  if b = 0L then (a, 1L, 0L)
  else
    let g, x1, y1 = egcd b (Int64.rem a b) in
    (g, y1, Int64.sub x1 (Int64.mul (Int64.div a b) y1))

let modinv a m =
  let _, x, _ = egcd a m in
  Int64.rem (Int64.add (Int64.rem x m) m) m

let bump facs p =
  match facs with
  | (q, c) :: tl when q = p -> (q, c + 1) :: tl
  | _ -> (p, 1) :: facs

let factorize n =
  let facs = ref [] in
  let v = ref n in
  let d = ref 2L in
  while Int64.mul !d !d <= !v do
    if Int64.rem !v !d = 0L then begin
      facs := bump !facs !d;
      v := Int64.div !v !d
    end
    else d := Int64.add !d (if !d = 2L then 1L else 2L)
  done;
  if !v > 1L then facs := bump !facs !v;
  !facs

let subgroup_log g h p q e n =
  let qe =
    let r = ref 1L in
    for _ = 1 to e do
      r := Int64.mul !r q
    done;
    !r
  in
  let gi = modpow g (Int64.div n qe) p in
  let hi = modpow h (Int64.div n qe) p in
  let qe1 =
    let r = ref 1L in
    for _ = 1 to e - 1 do
      r := Int64.mul !r q
    done;
    !r
  in
  let gamma = modpow gi qe1 p in
  let table = Hashtbl.create 1024 in
  let cur = ref 1L in
  let j = ref 0L in
  while !j < q do
    Hashtbl.add table !cur !j;
    cur := Int64.rem (Int64.mul !cur gamma) p;
    j := Int64.add !j 1L
  done;
  let xi = ref 0L in
  let hcur = ref hi in
  let qk = ref 1L in
  for k = 0 to e - 1 do
    let pw =
      let r = ref 1L in
      for _ = 1 to e - 1 - k do
        r := Int64.mul !r q
      done;
      !r
    in
    let dk = Hashtbl.find table (modpow !hcur pw p) in
    xi := Int64.add !xi (Int64.mul dk !qk);
    let sub = Int64.rem (Int64.mul dk !qk) (Int64.sub p 1L) in
    hcur := Int64.rem (Int64.mul !hcur (modinv (modpow gi sub p) p)) p;
    qk := Int64.mul !qk q
  done;
  !xi

let () =
  try
    let p = Scanf.scanf " %Ld" (fun x -> x) in
    let g = Scanf.scanf " %Ld" (fun x -> x) in
    let h = Scanf.scanf " %Ld" (fun x -> x) in
    let n = Int64.sub p 1L in
    let x = ref 0L in
    let m = ref 1L in
    List.iter
      (fun (q, e) ->
        let qe =
          let r = ref 1L in
          for _ = 1 to e do
            r := Int64.mul !r q
          done;
          !r
        in
        let xi = subgroup_log g h p q e n in
        let inv = modinv (Int64.rem !m qe) qe in
        let t =
          Int64.rem
            (Int64.add (Int64.rem (Int64.mul (Int64.sub xi !x) inv) qe) qe)
            qe
        in
        x := Int64.rem (Int64.add !x (Int64.mul !m t)) (Int64.mul !m qe);
        m := Int64.mul !m qe)
      (factorize n);
    print_string (Int64.to_string (Int64.rem (Int64.add (Int64.rem !x n) n) n))
  with End_of_file -> ()
