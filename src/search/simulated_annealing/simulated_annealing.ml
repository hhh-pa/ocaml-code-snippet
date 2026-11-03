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
  Int64.to_float shifted *. (1.0 /. 9007199254740992.0)

let attacking_pairs b =
  let n = Array.length b in
  let cnt = ref 0 in
  for i = 0 to n - 1 do
    let bi = b.(i) in
    for j = i + 1 to n - 1 do
      let bj = b.(j) in
      if bi = bj then incr cnt
      else if abs (bi - bj) = j - i then incr cnt
    done
  done;
  !cnt

let solve_case () =
  let n = Scanf.scanf " %d" (fun x -> x) in
  let board = Array.init n (fun _ -> Scanf.scanf " %d" (fun x -> x)) in
  let seed = Scanf.scanf " %Ld" (fun x -> x) in
  let temp = ref 100.0 in
  let cooling = 0.99 in
  let temp_min = 0.01 in
  let rng = make_lcg seed in
  let cur_cost = ref (attacking_pairs board) in
  let best_cost = ref !cur_cost in
  let best_board = Array.copy board in
  let steps = ref 0 in
  let max_steps = 5000 in
  while !temp > temp_min && !steps < max_steps do
    let r = next_int rng n in
    let c = ref (next_int rng n) in
    if !c = board.(r) then c := (!c + 1) mod n;
    let old = board.(r) in
    board.(r) <- !c;
    let new_cost = attacking_pairs board in
    let delta = new_cost - !cur_cost in
    if delta < 0 || next_double rng < exp (-. (float_of_int delta) /. !temp) then begin
      cur_cost := new_cost;
      if !cur_cost < !best_cost then begin
        best_cost := !cur_cost;
        Array.blit board 0 best_board 0 n
      end
    end else begin
      board.(r) <- old
    end;
    temp := !temp *. cooling;
    incr steps
  done;
  for i = 0 to n - 1 do
    if i > 0 then print_string " ";
    print_int best_board.(i)
  done;
  print_newline ();
  Printf.printf "%d\n" !best_cost

let () =
  try
    let t = Scanf.scanf " %d" (fun x -> x) in
    for _ = 0 to t - 1 do
      solve_case ()
    done
  with End_of_file -> ()
