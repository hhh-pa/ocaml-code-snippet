let mult = 6364136223846793005L
let add = 1442695040888963407L

type lcg = { mutable s : int64 }

let make_lcg seed = { s = seed }

let next_lcg rng =
  rng.s <- Int64.add (Int64.mul rng.s mult) add;
  rng.s

let next_int rng n =
  let v = next_lcg rng in
  Int64.to_int (Int64.unsigned_rem v (Int64.of_int n))

let next_double rng =
  let v = next_lcg rng in
  let shifted = Int64.shift_right_logical v 11 in
  Int64.to_float shifted *. 1.1102230246251565e-16

let solve_case () =
  let s = Scanf.scanf " %d" (fun x -> x) in
  let o = Scanf.scanf " %d" (fun x -> x) in
  let n = Scanf.scanf " %d" (fun x -> x) in
  let tl = Scanf.scanf " %d" (fun x -> x) in
  let seed = Scanf.scanf " %Ld" (fun x -> x) in
  let tr = Array.make_matrix s s 0.0 in
  for i = 0 to s - 1 do
    for j = 0 to s - 1 do
      let num = Scanf.scanf " %f" (fun x -> x) in
      let den = Scanf.scanf " %f" (fun x -> x) in
      tr.(i).(j) <- num /. den
    done
  done;
  let se = Array.make_matrix s o 0.0 in
  for i = 0 to s - 1 do
    for j = 0 to o - 1 do
      let num = Scanf.scanf " %f" (fun x -> x) in
      let den = Scanf.scanf " %f" (fun x -> x) in
      se.(i).(j) <- num /. den
    done
  done;
  let obs = Array.init tl (fun _ -> Scanf.scanf " %d" (fun x -> x)) in
  let rng = make_lcg seed in
  let parts = Array.init n (fun _ -> next_int rng s) in
  Printf.printf "PF\n";
  for k = 0 to tl - 1 do
    for i = 0 to n - 1 do
      let u = next_double rng in
      let c = ref 0.0 in
      let ns = ref (s - 1) in
      let row = tr.(parts.(i)) in
      let found = ref false in
      let j = ref 0 in
      while !j < s && not !found do
        c := !c +. row.(!j);
        if u < !c then begin
          ns := !j;
          found := true
        end;
        incr j
      done;
      parts.(i) <- !ns
    done;
    let w = Array.init n (fun i -> se.(parts.(i)).(obs.(k))) in
    let tot = ref 0.0 in
    for i = 0 to n - 1 do
      tot := !tot +. w.(i)
    done;
    let np = Array.make n 0 in
    if !tot = 0.0 then begin
      for i = 0 to n - 1 do
        np.(i) <- next_int rng s
      done
    end else begin
      let cw = Array.make (n + 1) 0.0 in
      let inv_tot = 1.0 /. !tot in
      for i = 0 to n - 1 do
        cw.(i + 1) <- cw.(i) +. w.(i) *. inv_tot
      done;
      for i = 0 to n - 1 do
        let u = next_double rng in
        let pick = ref (n - 1) in
        let found = ref false in
        let j = ref 0 in
        while !j < n && not !found do
          if u < cw.(!j + 1) then begin
            pick := !j;
            found := true
          end;
          incr j
        done;
        np.(i) <- parts.(!pick)
      done
    end;
    Array.blit np 0 parts 0 n;
    let hist = Array.make s 0 in
    for i = 0 to n - 1 do
      hist.(parts.(i)) <- hist.(parts.(i)) + 1
    done;
    for j = 0 to s - 1 do
      if j > 0 then print_string " ";
      print_int hist.(j)
    done;
    print_newline ()
  done

let () =
  try
    let t = Scanf.scanf " %d" (fun x -> x) in
    for _ = 0 to t - 1 do
      solve_case ()
    done
  with End_of_file -> ()
