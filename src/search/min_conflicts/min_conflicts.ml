let conflicts_at b row col =
  let n = Array.length b in
  let c = ref 0 in
  for i = 0 to n - 1 do
    if i <> row then begin
      let bi = b.(i) in
      if bi = col then incr c
      else if abs (bi - col) = abs (i - row) then incr c
    end
  done;
  !c

let total_pairs b =
  let n = Array.length b in
  let c = ref 0 in
  for i = 0 to n - 1 do
    let bi = b.(i) in
    for j = i + 1 to n - 1 do
      let bj = b.(j) in
      if bi = bj then incr c
      else if abs (bi - bj) = j - i then incr c
    done
  done;
  !c

let solve_case () =
  let n = Scanf.scanf " %d" (fun x -> x) in
  let ms = Scanf.scanf " %d" (fun x -> x) in
  let b = Array.init n (fun _ -> Scanf.scanf " %d" (fun x -> x)) in
  for _ = 0 to ms - 1 do
    let vr = ref (-1) in
    let vc = ref (-1) in
    for i = 0 to n - 1 do
      let c = conflicts_at b i b.(i) in
      if c > 0 && c > !vc then begin
        vc := c;
        vr := i
      end
    done;
    if !vr >= 0 then begin
      let r = !vr in
      let bc = ref b.(r) in
      let bv = ref (conflicts_at b r !bc) in
      for c = 0 to n - 1 do
        let v = conflicts_at b r c in
        if v < !bv then begin
          bv := v;
          bc := c
        end
      done;
      b.(r) <- !bc
    end
  done;
  for i = 0 to n - 1 do
    if i > 0 then print_string " ";
    print_int b.(i)
  done;
  print_newline ();
  Printf.printf "%d\n" (total_pairs b)

let () =
  try
    let t = Scanf.scanf " %d" (fun x -> x) in
    for _ = 0 to t - 1 do
      solve_case ()
    done
  with End_of_file -> ()
