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

let solve_case () =
  let v_cnt = Scanf.scanf " %d" (fun x -> x) in
  let c_cnt = Scanf.scanf " %d" (fun x -> x) in
  let mf = Scanf.scanf " %d" (fun x -> x) in
  let pn = Scanf.scanf " %d" (fun x -> x) in
  let pd = Scanf.scanf " %d" (fun x -> x) in
  let seed = Scanf.scanf " %Ld" (fun x -> x) in
  let clauses = Array.init c_cnt (fun _ ->
    let k = Scanf.scanf " %d" (fun x -> x) in
    Array.init k (fun _ -> Scanf.scanf " %d" (fun x -> x))
  ) in
  let rng = make_lcg seed in
  let asn = Array.init v_cnt (fun _ -> next_int rng 2) in

  let sat_lit l =
    let v = abs l - 1 in
    (asn.(v) = 1) = (l > 0)
  in

  let sat_cls c =
    let found = ref false in
    let i = ref 0 in
    let len = Array.length c in
    while !i < len && not !found do
      if sat_lit c.(!i) then found := true;
      incr i
    done;
    !found
  in

  let all_sat () =
    let all_ok = ref true in
    let i = ref 0 in
    while !i < c_cnt && !all_ok do
      if not (sat_cls clauses.(!i)) then all_ok := false;
      incr i
    done;
    !all_ok
  in

  let solved = ref (all_sat ()) in
  let f = ref 0 in
  while !f < mf && not !solved do
    let unsat = ref [] in
    for i = 0 to c_cnt - 1 do
      if not (sat_cls clauses.(i)) then unsat := i :: !unsat
    done;
    let unsat_arr = Array.of_list (List.rev !unsat) in
    let ci = unsat_arr.(next_int rng (Array.length unsat_arr)) in
    let cl = clauses.(ci) in
    let pick = ref (-1) in
    let rand_val = next_lcg rng in
    let rem = Int64.unsigned_rem rand_val (Int64.of_int pd) in
    if rem < Int64.of_int pn then begin
      let idx = next_int rng (Array.length cl) in
      pick := abs cl.(idx) - 1
    end else begin
      let best = ref 1000000000 in
      for li = 0 to Array.length cl - 1 do
        let l = cl.(li) in
        let v = abs l - 1 in
        let brk = ref 0 in
        asn.(v) <- asn.(v) lxor 1;
        for c2_i = 0 to c_cnt - 1 do
          let c2 = clauses.(c2_i) in
          if not (sat_cls c2) then begin
            asn.(v) <- asn.(v) lxor 1;
            let was = sat_cls c2 in
            asn.(v) <- asn.(v) lxor 1;
            if was then incr brk
          end
        done;
        asn.(v) <- asn.(v) lxor 1;
        if !brk < !best then begin
          best := !brk;
          pick := v
        end
      done
    end;
    asn.(!pick) <- asn.(!pick) lxor 1;
    if all_sat () then solved := true;
    incr f
  done;

  Printf.printf "WALKSAT %s" (if !solved then "SAT" else "UNKNOWN");
  for v = 0 to v_cnt - 1 do
    Printf.printf " %d" asn.(v)
  done;
  print_newline ()

let () =
  try
    let t = Scanf.scanf " %d" (fun x -> x) in
    for _ = 0 to t - 1 do
      solve_case ()
    done
  with End_of_file -> ()
