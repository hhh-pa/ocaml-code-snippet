(* Series acceleration (NR 5.3): epsilon algorithm and Levin u-transform. Port of the C++ reference. *)

let fmt6 v =
  let w = if abs_float v < 5e-7 then 0.0 else v in
  Printf.sprintf "%.6f" (w +. 0.0)

let small = 2.2250738585072014e-307
let big = 1.7976931348623157e308

let () =
  let toks =
    String.split_on_char ' '
      (String.concat " "
         (let rec rd acc =
            try rd (input_line stdin :: acc) with End_of_file -> List.rev acc
          in
          rd []))
  in
  let toks = Array.of_list (List.filter (fun s -> s <> "") toks) in
  if Array.length toks = 0 then exit 0;
  let pos = ref 0 in
  let next () =
    let v = toks.(!pos) in
    incr pos; v
  in
  let op = next () in
  let n = int_of_string (next ()) in
  let a = Array.init n (fun _ -> float_of_string (next ())) in
  let sum = ref 0.0 in
  let nn = ref 0 in
  let res = ref 0.0 in
  if op = "EPSALG" then (
    let e = Array.make n 0.0 in
    for k = 0 to n - 1 do
      sum := !sum +. a.(k);
      e.(!nn) <- !sum;
      let temp2 = ref 0.0 in
      for j = !nn downto 1 do
        let temp1 = !temp2 in
        temp2 := e.(j - 1);
        let diff = e.(j) -. !temp2 in
        if abs_float diff <= small then e.(j - 1) <- big
        else e.(j - 1) <- temp1 +. 1.0 /. diff
      done;
      incr nn;
      res := if !nn mod 2 = 1 then e.(0) else e.(1)
    done)
  else (
    let numer = Array.make n 0.0 and denom = Array.make n 0.0 in
    let lastval = ref 0.0 in
    for k = 0 to n - 1 do
      sum := !sum +. a.(k);
      let omega = (1.0 +. float !nn) *. a.(k) in
      let term = ref (1.0 /. (1.0 +. float !nn)) in
      denom.(!nn) <- !term /. omega;
      numer.(!nn) <- !sum *. denom.(!nn);
      if !nn > 0 then (
        let ratio = (1.0 +. float !nn -. 1.0) *. !term in
        for j = 1 to !nn do
          let fact = (float (!nn - j) +. 1.0) *. !term in
          numer.(!nn - j) <- numer.(!nn - j + 1) -. fact *. numer.(!nn - j);
          denom.(!nn - j) <- denom.(!nn - j + 1) -. fact *. denom.(!nn - j);
          term := !term *. ratio
        done);
      incr nn;
      res := if abs_float denom.(0) < small then !lastval else numer.(0) /. denom.(0);
      lastval := !res
    done);
  print_string (fmt6 (if abs_float !res < 5e-7 then 0.0 else !res));
  print_newline ()
