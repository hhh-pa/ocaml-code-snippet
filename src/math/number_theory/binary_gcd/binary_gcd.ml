let bgcd a b =
  if a = 0L then (if b >= 0L then b else Int64.neg b)
  else if b = 0L then (if a >= 0L then a else Int64.neg a)
  else
    let a = ref (if a < 0L then Int64.neg a else a) in
    let b = ref (if b < 0L then Int64.neg b else b) in
    let shift = ref 0 in
    while Int64.logand (Int64.logor !a !b) 1L = 0L do
      a := Int64.shift_right_logical !a 1;
      b := Int64.shift_right_logical !b 1;
      incr shift
    done;
    while Int64.logand !a 1L = 0L do
      a := Int64.shift_right_logical !a 1
    done;
    while !b <> 0L do
      while Int64.logand !b 1L = 0L do
        b := Int64.shift_right_logical !b 1
      done;
      if !a > !b then begin
        let tmp = !a in
        a := !b;
        b := tmp
      end;
      b := Int64.sub !b !a
    done;
    Int64.shift_left !a !shift

let () =
  let tokens = ref [] in
  let rec read_all () =
    try
      let v = Scanf.scanf " %Ld" (fun x -> x) in
      tokens := v :: !tokens;
      read_all ()
    with End_of_file -> ()
  in
  read_all ();
  let vals = Array.of_list (List.rev !tokens) in
  let cnt = Array.length vals in
  if cnt = 0 then ()
  else if cnt = 2 then
    Printf.printf "%Ld\n" (bgcd vals.(0) vals.(1))
  else
    let t = Int64.to_int vals.(0) in
    if t > 0 && cnt = 2 * t + 1 then
      for i = 0 to t - 1 do
        Printf.printf "%Ld\n" (bgcd vals.(1 + 2 * i) vals.(1 + 2 * i + 1))
      done
    else
      let i = ref 0 in
      while !i + 1 < cnt do
        Printf.printf "%Ld\n" (bgcd vals.(!i) vals.(!i + 1));
        i := !i + 2
      done
