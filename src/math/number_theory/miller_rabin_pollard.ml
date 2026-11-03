let mod_mul a b m =
  if m <= 3_000_000_000L then
    Int64.rem (Int64.mul a b) m
  else
    let rec loop cur_a cur_b acc =
      if cur_b = 0L then acc
      else
        let new_acc =
          if Int64.logand cur_b 1L = 1L then
            let sum = Int64.add acc cur_a in
            if sum >= m || sum < 0L then Int64.rem sum m else sum
          else acc
        in
        let double_a = Int64.add cur_a cur_a in
        let new_a =
          if double_a >= m || double_a < 0L then Int64.rem double_a m else double_a
        in
        loop new_a (Int64.shift_right_logical cur_b 1) new_acc
    in
    loop (Int64.rem a m) b 0L

let mod_pow base exp m =
  let rec loop b e acc =
    if e = 0L then acc
    else
      let new_acc = if Int64.logand e 1L = 1L then mod_mul acc b m else acc in
      let new_b = mod_mul b b m in
      loop new_b (Int64.shift_right_logical e 1) new_acc
  in
  loop (Int64.rem base m) exp 1L

let rec gcd a b =
  if b = 0L then a else gcd b (Int64.rem a b)

let miller_rabin n =
  if n < 2L then false
  else if n = 2L || n = 3L then true
  else if Int64.rem n 2L = 0L then false
  else begin
    let d = ref (Int64.sub n 1L) in
    let s = ref 0 in
    while Int64.logand !d 1L = 0L do
      d := Int64.shift_right_logical !d 1;
      incr s
    done;

    let bases = [2L; 3L; 5L; 7L; 11L; 13L; 17L; 19L; 23L; 29L; 31L; 37L] in
    let is_prime = ref true in
    let rec check_bases = function
      | [] -> ()
      | a :: rest ->
        if n <= a then ()
        else begin
          let x = ref (mod_pow a !d n) in
          let n_minus_1 = Int64.sub n 1L in
          if !x = 1L || !x = n_minus_1 then
            check_bases rest
          else begin
            let composite = ref true in
            let r = ref 1 in
            while !r < !s && !composite do
              x := mod_mul !x !x n;
              if !x = n_minus_1 then composite := false;
              incr r
            done;
            if !composite then
              is_prime := false
            else
              check_bases rest
          end
        end
    in
    check_bases bases;
    !is_prime
  end

let pollard_rho n =
  if Int64.rem n 2L = 0L then 2L
  else begin
    let x = ref 2L in
    let y = ref 2L in
    let d = ref 1L in
    let c = ref 1L in
    let f v =
      Int64.rem (Int64.add (mod_mul v v n) !c) n
    in
    while !d = 1L do
      x := f !x;
      y := f (f !y);
      let diff = if !x > !y then Int64.sub !x !y else Int64.sub !y !x in
      d := gcd diff n;
      if !d = n then begin
        x := 2L;
        y := 2L;
        c := Int64.add !c 1L;
        d := 1L
      end
    done;
    !d
  end

let rec factorize n factors =
  if n = 1L then ()
  else if miller_rabin n then
    factors := n :: !factors
  else begin
    let d = pollard_rho n in
    factorize d factors;
    factorize (Int64.div n d) factors
  end

let () =
  try
    let t = Scanf.scanf " %d" (fun x -> x) in
    for _ = 1 to t do
      let n = Scanf.scanf " %Ld" (fun x -> x) in
      if miller_rabin n then
        Printf.printf "PRIME\n"
      else begin
        let factors = ref [] in
        factorize n factors;
        let sorted = List.sort compare !factors in
        Printf.printf "COMPOSITE";
        List.iter (fun f -> Printf.printf " %Ld" f) sorted;
        Printf.printf "\n"
      end
    done
  with End_of_file -> ()
