let kth bit n k_in =
  let k = ref k_in in
  let i = ref 0 in
  let step = ref 1 in
  while !step * 2 <= n do
    step := !step * 2
  done;
  while !step > 0 do
    let j = !i + !step in
    if j <= n && bit.(j) < !k then begin
      k := !k - bit.(j);
      i := j
    end;
    step := !step / 2
  done;
  !i + 1

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let m = Scanf.scanf " %Ld" (fun x -> x) in
    let bit = Array.make (n + 1) 0 in
    let add i d =
      let idx = ref i in
      while !idx <= n do
        bit.(!idx) <- bit.(!idx) + d;
        idx := !idx + (!idx land (- !idx))
      done
    in
    for i = 1 to n do
      add i 1
    done;
    let pos = ref 0L in
    let rem = ref n in
    let first = ref true in
    while !rem > 0 do
      pos := Int64.rem (Int64.add !pos (Int64.sub m 1L)) (Int64.of_int !rem);
      let idx = kth bit n (Int64.to_int !pos + 1) in
      if not !first then print_string " ";
      first := false;
      Printf.printf "%d" idx;
      add idx (-1);
      decr rem
    done;
    print_newline ()
  with End_of_file -> ()
