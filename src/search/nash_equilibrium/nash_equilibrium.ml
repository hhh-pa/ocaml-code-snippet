let rec gcdll a b =
  let a = if a < 0L then Int64.neg a else a in
  let b = if b < 0L then Int64.neg b else b in
  if b = 0L then (if a = 0L then 1L else a)
  else gcdll b (Int64.rem a b)

let solve_case () =
  let r = Scanf.scanf " %d" (fun x -> x) in
  let c = Scanf.scanf " %d" (fun x -> x) in
  let u1 = Array.make_matrix r c 0L in
  let u2 = Array.make_matrix r c 0L in
  for i = 0 to r - 1 do
    for j = 0 to c - 1 do
      u1.(i).(j) <- Scanf.scanf " %Ld" (fun x -> x);
      u2.(i).(j) <- Scanf.scanf " %Ld" (fun x -> x)
    done
  done;
  let rows = Array.make r true in
  let cols = Array.make c true in
  let changed = ref true in
  while !changed do
    changed := false;
    let nrows = Array.copy rows in
    for i = 0 to r - 1 do
      if rows.(i) then begin
        let k = ref 0 in
        let dominated = ref false in
        while !k < r && not !dominated do
          if !k <> i && rows.(!k) then begin
            let dom = ref true in
            let j = ref 0 in
            while !j < c && !dom do
              if cols.(!j) then begin
                if not (u1.(!k).(!j) > u1.(i).(!j)) then dom := false
              end;
              incr j
            done;
            if !dom then begin
              nrows.(i) <- false;
              changed := true;
              dominated := true
            end
          end;
          incr k
        done
      end
    done;
    Array.blit nrows 0 rows 0 r;

    let ncols = Array.copy cols in
    for j = 0 to c - 1 do
      if cols.(j) then begin
        let k = ref 0 in
        let dominated = ref false in
        while !k < c && not !dominated do
          if !k <> j && cols.(!k) then begin
            let dom = ref true in
            let i = ref 0 in
            while !i < r && !dom do
              if rows.(!i) then begin
                if not (u2.(!i).(!k) > u2.(!i).(j)) then dom := false
              end;
              incr i
            done;
            if !dom then begin
              ncols.(j) <- false;
              changed := true;
              dominated := true
            end
          end;
          incr k
        done
      end
    done;
    Array.blit ncols 0 cols 0 c
  done;

  let rr = ref [] in
  for i = 0 to r - 1 do
    if rows.(i) then rr := i :: !rr
  done;
  let rr = List.rev !rr in
  let cc = ref [] in
  for j = 0 to c - 1 do
    if cols.(j) then cc := j :: !cc
  done;
  let cc = List.rev !cc in

  let pure = ref [] in
  List.iter (fun i ->
    List.iter (fun j ->
      let b1 = List.for_all (fun k -> u1.(k).(j) <= u1.(i).(j)) rr in
      let b2 = List.for_all (fun k -> u2.(i).(k) <= u2.(i).(j)) cc in
      if b1 && b2 then pure := (i, j) :: !pure
    ) cc
  ) rr;
  let pure = List.rev !pure in

  Printf.printf "NASH %d %d\n" (List.length rr) (List.length cc);
  Printf.printf "PURE %d" (List.length pure);
  List.iter (fun (i, j) -> Printf.printf " %d:%d" i j) pure;
  Printf.printf "\n";

  if List.length rr = 2 && List.length cc = 2 then begin
    let a = List.nth rr 0 in
    let b = List.nth rr 1 in
    let c0 = List.nth cc 0 in
    let d = List.nth cc 1 in
    let pn = ref (Int64.sub u2.(b).(d) u2.(b).(c0)) in
    let pd = ref (Int64.sub (Int64.sub u2.(a).(c0) u2.(b).(c0)) (Int64.sub u2.(a).(d) u2.(b).(d))) in
    let qn = ref (Int64.sub u1.(b).(d) u1.(a).(d)) in
    let qd = ref (Int64.sub (Int64.sub u1.(a).(c0) u1.(a).(d)) (Int64.sub u1.(b).(c0) u1.(b).(d))) in
    let ok = ref (!pd <> 0L && !qd <> 0L) in
    if !ok then begin
      if !pd < 0L then begin
        pn := Int64.neg !pn;
        pd := Int64.neg !pd
      end;
      if !qd < 0L then begin
        qn := Int64.neg !qn;
        qd := Int64.neg !qd
      end;
      let gp = gcdll !pn !pd in
      let gq = gcdll !qn !qd in
      pn := Int64.div !pn gp;
      pd := Int64.div !pd gp;
      qn := Int64.div !qn gq;
      qd := Int64.div !qd gq;
      if not (!pn > 0L && !pn < !pd && !qn > 0L && !qn < !qd) then
        ok := false
    end;
    if not !ok then
      Printf.printf "MIXED NONE\n"
    else
      Printf.printf "MIXED %Ld/%Ld %Ld/%Ld\n" !pn !pd !qn !qd
  end else
    Printf.printf "MIXED NONE\n"

let () =
  try
    let t = Scanf.scanf " %d" (fun x -> x) in
    for _ = 0 to t - 1 do
      solve_case ()
    done
  with End_of_file -> ()
