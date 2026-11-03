module LCG = struct
  type t = { mutable s : int64 }
  let create s = { s }
  let next t =
    t.s <- Int64.add (Int64.mul t.s 6364136223846793005L) 1442695040888963407L;
    t.s
  let next_int t n =
    let v = next t in
    Int64.to_int (Int64.unsigned_rem v (Int64.of_int n))
end

let attacking b =
  let n = Array.length b in
  let c = ref 0 in
  for i = 0 to n - 1 do
    for j = i + 1 to n - 1 do
      if b.(i) = b.(j) then incr c
      else if abs (b.(i) - b.(j)) = j - i then incr c
    done
  done;
  !c

let solve_case () =
  let n = Scanf.scanf " %d" (fun x -> x) in
  let k = Scanf.scanf " %d" (fun x -> x) in
  let seed = Scanf.scanf " %Ld" (fun x -> x) in
  let rng = LCG.create seed in

  let beam = ref [] in
  for _ = 0 to k - 1 do
    let b = Array.make n 0 in
    for j = 0 to n - 1 do
      b.(j) <- LCG.next_int rng n
    done;
    beam := b :: !beam
  done;
  beam := List.rev !beam;

  let best_cost = ref (1 lsl 30) in
  let best_board = ref [||] in
  List.iter (fun b ->
    let c = attacking b in
    if c < !best_cost then begin
      best_cost := c;
      best_board := Array.copy b
    end
  ) !beam;

  if !best_cost <> 0 then begin
    let iter_count = ref 0 in
    while !iter_count < 100 && !best_cost <> 0 do
      let succs = ref [] in
      List.iter (fun b ->
        for r = 0 to n - 1 do
          for c2 = 0 to n - 1 do
            if c2 <> b.(r) then begin
              let nb = Array.copy b in
              nb.(r) <- c2;
              succs := (attacking nb, nb) :: !succs
            end
          done
        done
      ) !beam;
      let sorted_succs = List.sort (fun (c1, b1) (c2, b2) ->
        if c1 <> c2 then compare c1 c2
        else compare b1 b2
      ) !succs in
      let new_beam = ref [] in
      let count = ref 0 in
      let rec take = function
        | [] -> ()
        | (_, nb) :: rest ->
            if !count < k then begin
              if not (List.exists (fun ex -> ex = nb) !new_beam) then begin
                new_beam := nb :: !new_beam;
                incr count
              end;
              take rest
            end
      in
      take sorted_succs;
      beam := List.rev !new_beam;
      List.iter (fun b ->
        let c = attacking b in
        if c < !best_cost then begin
          best_cost := c;
          best_board := Array.copy b
        end
      ) !beam;
      incr iter_count
    done
  end;

  for i = 0 to n - 1 do
    if i > 0 then Printf.printf " ";
    Printf.printf "%d" (!best_board).(i)
  done;
  Printf.printf "\n%d\n" !best_cost

let () =
  try
    let t = Scanf.scanf " %d" (fun x -> x) in
    for _ = 0 to t - 1 do
      solve_case ()
    done
  with End_of_file -> ()
