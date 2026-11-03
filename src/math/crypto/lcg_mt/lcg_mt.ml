let mask32 = 0xFFFFFFFFL

type mt_state = {
  mt: int64 array;
  mutable idx: int;
}

let mt_create seed =
  let state = { mt = Array.make 624 0L; idx = 624 } in
  state.mt.(0) <- Int64.logand seed mask32;
  for i = 1 to 623 do
    let prev = state.mt.(i - 1) in
    let term = Int64.logxor prev (Int64.shift_right_logical prev 30) in
    let v = Int64.add (Int64.mul 1812433253L term) (Int64.of_int i) in
    state.mt.(i) <- Int64.logand v mask32
  done;
  state

let mt_twist state =
  for i = 0 to 623 do
    let y =
      Int64.logor
        (Int64.logand state.mt.(i) 0x80000000L)
        (Int64.logand state.mt.((i + 1) mod 624) 0x7FFFFFFFL)
    in
    let half_y = Int64.shift_right_logical y 1 in
    let next = Int64.logxor state.mt.((i + 397) mod 624) half_y in
    state.mt.(i) <- (if Int64.logand y 1L = 1L then Int64.logxor next 0x9908B0DFL else next)
  done;
  state.idx <- 0

let mt_next state =
  if state.idx >= 624 then mt_twist state;
  let y = ref state.mt.(state.idx) in
  state.idx <- state.idx + 1;
  y := Int64.logxor !y (Int64.shift_right_logical !y 11);
  y := Int64.logxor !y (Int64.logand (Int64.shift_left !y 7) 0x9D2C5680L);
  y := Int64.logxor !y (Int64.logand (Int64.shift_left !y 15) 0xEFC60000L);
  y := Int64.logxor !y (Int64.shift_right_logical !y 18);
  Int64.logand !y mask32

let () =
  try
    let mode = Scanf.scanf " %s" (fun s -> s) in
    if mode = "L" then begin
      let a = Scanf.scanf " %Ld" (fun x -> x) in
      let c = Scanf.scanf " %Ld" (fun x -> x) in
      let m = Scanf.scanf " %Ld" (fun x -> x) in
      let seed = Scanf.scanf " %Ld" (fun x -> x) in
      let n = Scanf.scanf " %d" (fun x -> x) in
      let x = ref seed in
      for i = 0 to n - 1 do
        x := Int64.rem (Int64.add (Int64.mul a !x) c) m;
        if i > 0 then print_string " ";
        Printf.printf "%Ld" !x
      done;
      print_newline ()
    end else begin
      let seed = Scanf.scanf " %Ld" (fun x -> x) in
      let n = Scanf.scanf " %d" (fun x -> x) in
      let mt = mt_create seed in
      for i = 0 to n - 1 do
        let v = mt_next mt in
        if i > 0 then print_string " ";
        Printf.printf "%Ld" v
      done;
      print_newline ()
    end
  with End_of_file -> ()
