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

let tonelli a0 p =
  let a = Int64.rem (Int64.add (Int64.rem a0 p) p) p in
  if a = 0L then 0L
  else if modpow a (Int64.div (Int64.sub p 1L) 2L) p <> 1L then -1L
  else if Int64.rem p 4L = 3L then modpow a (Int64.div (Int64.add p 1L) 4L) p
  else
    let q = ref (Int64.sub p 1L) in
    let s = ref 0L in
    while Int64.rem !q 2L = 0L do
      q := Int64.div !q 2L;
      s := Int64.add !s 1L
    done;
    let z = ref 2L in
    while modpow !z (Int64.div (Int64.sub p 1L) 2L) p <> Int64.sub p 1L do
      z := Int64.add !z 1L
    done;
    let c = ref (modpow !z !q p) in
    let r = ref (modpow a (Int64.div (Int64.add !q 1L) 2L) p) in
    let t = ref (modpow a !q p) in
    let m = ref !s in
    while !t <> 1L do
      let t2 = ref (Int64.rem (Int64.mul !t !t) p) in
      let i = ref 1L in
      while !t2 <> 1L do
        t2 := Int64.rem (Int64.mul !t2 !t2) p;
        i := Int64.add !i 1L
      done;
      let b =
        modpow !c
          (Int64.shift_left 1L (Int64.to_int (Int64.sub (Int64.sub !m !i) 1L)))
          p
      in
      r := Int64.rem (Int64.mul !r b) p;
      c := Int64.rem (Int64.mul b b) p;
      t := Int64.rem (Int64.mul !t !c) p;
      m := !i
    done;
    !r

let hensel a p e r0 =
  let r = ref r0 in
  let m = ref p in
  for _ = 1 to e - 1 do
    m := Int64.mul !m p;
    let fr =
      Int64.rem (Int64.add (Int64.rem (Int64.sub (Int64.mul !r !r) a) !m) !m) !m
    in
    r :=
      Int64.rem
        (Int64.add
           (Int64.rem
              (Int64.sub !r
                 (Int64.mul fr (modinv (Int64.rem (Int64.mul 2L !r) !m) !m)))
              !m)
           !m)
        !m
  done;
  !r

let rec prime_power_root a0 p e =
  let n =
    let r = ref 1L in
    for _ = 1 to e do
      r := Int64.mul !r p
    done;
    !r
  in
  let a = Int64.rem (Int64.add (Int64.rem a0 n) n) n in
  if Int64.rem a p = 0L then
    begin if a = 0L then 0L
    else
      let va = ref 0L in
      let t = ref a in
      while Int64.rem !t p = 0L do
        t := Int64.div !t p;
        va := Int64.add !va 1L
      done;
      if Int64.rem !va 2L = 1L then -1L
      else
        let sub = prime_power_root !t p (e - Int64.to_int !va) in
        if sub = -1L then -1L
        else
          let pw =
            let r = ref 1L in
            for _ = 1 to Int64.to_int (Int64.div !va 2L) do
              r := Int64.mul !r p
            done;
            !r
          in
          let r = Int64.rem (Int64.mul sub pw) n in
          min r (Int64.sub n r)
    end
  else
    let r0 = tonelli a p in
    if r0 = -1L then -1L
    else
      let r = hensel a p e r0 in
      min r (Int64.sub n r)

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

let () =
  try
    let a = Scanf.scanf " %Ld" (fun x -> x) in
    let n = Scanf.scanf " %Ld" (fun x -> x) in
    let facs = factorize n in
    if List.length facs = 1 then
      let p, e = List.hd facs in
      print_string (Int64.to_string (prime_power_root a p e))
    else if List.length facs = 2 then
      match facs with
      | [ (p, ep); (q, eq) ] when ep = 1 && eq = 1 ->
          let rp = tonelli a p in
          let rq = tonelli a q in
          if rp = -1L || rq = -1L then print_string "-1"
          else
            let best = ref n in
            List.iter
              (fun sp ->
                List.iter
                  (fun sq ->
                    let r =
                      Int64.rem
                        (Int64.add sp
                           (Int64.mul p
                              (Int64.add
                                 (Int64.rem
                                    (Int64.mul (Int64.sub sq sp)
                                       (modinv (Int64.rem p q) q))
                                    q)
                                 q)))
                        n
                    in
                    if r < !best then best := r)
                  [ rq; Int64.rem (Int64.sub q rq) q ])
              [ rp; Int64.rem (Int64.sub p rp) p ];
            print_string (Int64.to_string !best)
      | _ -> print_string "-1"
    else print_string "-1"
  with End_of_file -> ()
