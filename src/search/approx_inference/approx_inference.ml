module LCG = struct
  type t = { mutable s : int64 }
  let create s = { s }
  let next t =
    t.s <- Int64.add (Int64.mul t.s 6364136223846793005L) 1442695040888963407L;
    t.s
  let next_int t n =
    let v = next t in
    Int64.to_int (Int64.unsigned_rem v (Int64.of_int n))
  let next_double t =
    let v = next t in
    let shifted = Int64.shift_right_logical v 11 in
    Int64.to_float shifted *. 1.1102230246251565e-16
end

let solve () =
  let n = Scanf.scanf " %d" (fun x -> x) in
  let parents = Array.make n [||] in
  let cpt = Array.make n [||] in
  let children = Array.make n [] in
  for v = 0 to n - 1 do
    let k = Scanf.scanf " %d" (fun x -> x) in
    let p_arr = Array.make k 0 in
    for j = 0 to k - 1 do
      let p = Scanf.scanf " %d" (fun x -> x) in
      p_arr.(j) <- p;
      children.(p) <- v :: children.(p)
    done;
    parents.(v) <- p_arr;
    let rows = 1 lsl k in
    let c_arr = Array.make rows 0.0 in
    for r = 0 to rows - 1 do
      let num = Scanf.scanf " %f" (fun x -> x) in
      let den = Scanf.scanf " %f" (fun x -> x) in
      c_arr.(r) <- num /. den
    done;
    cpt.(v) <- c_arr
  done;
  for v = 0 to n - 1 do
    children.(v) <- List.rev children.(v)
  done;
  let q = Scanf.scanf " %d" (fun x -> x) in
  let ne = Scanf.scanf " %d" (fun x -> x) in
  let evid = Array.make n (-1) in
  for _ = 0 to ne - 1 do
    let e = Scanf.scanf " %d" (fun x -> x) in
    let vv = Scanf.scanf " %d" (fun x -> x) in
    evid.(e) <- vv
  done;
  let s_samples = Scanf.scanf " %d" (fun x -> x) in
  let seed = Scanf.scanf " %Ld" (fun x -> x) in

  let prow v asn =
    let k = Array.length parents.(v) in
    let row = ref 0 in
    for j = 0 to k - 1 do
      let p_idx = parents.(v).(j) in
      row := !row lor (asn.(p_idx) lsl (k - 1 - j))
    done;
    cpt.(v).(!row)
  in

  (* Rejection sampling *)
  let rng = LCG.create seed in
  let acc = ref 0 in
  let qt = ref 0 in
  let asn = Array.make n 0 in
  for _ = 0 to s_samples - 1 do
    for v = 0 to n - 1 do
      asn.(v) <- if LCG.next_double rng < prow v asn then 1 else 0
    done;
    let ok = ref true in
    for v = 0 to n - 1 do
      if evid.(v) >= 0 && asn.(v) <> evid.(v) then ok := false
    done;
    if !ok then begin
      incr acc;
      if asn.(q) = 1 then incr qt
    end
  done;
  Printf.printf "REJECT %d %d\n" !acc !qt;

  (* Likelihood weighting *)
  let rng = LCG.create seed in
  let wt = ref 0.0 in
  let wq = ref 0.0 in
  let asn = Array.make n 0 in
  for _ = 0 to s_samples - 1 do
    let w = ref 1.0 in
    for v = 0 to n - 1 do
      if evid.(v) >= 0 then begin
        asn.(v) <- evid.(v);
        let p = prow v asn in
        w := !w *. (if evid.(v) = 1 then p else 1.0 -. p)
      end else begin
        asn.(v) <- if LCG.next_double rng < prow v asn then 1 else 0
      end
    done;
    wt := !wt +. !w;
    if asn.(q) = 1 then wq := !wq +. !w
  done;
  let est = if !wt = 0.0 then 0.0 else !wq /. !wt in
  Printf.printf "LIKELIHOOD %.6f\n" est;

  (* Gibbs sampling *)
  let rng = LCG.create seed in
  let asn = Array.make n 0 in
  for v = 0 to n - 1 do
    asn.(v) <- if evid.(v) >= 0 then evid.(v) else LCG.next_int rng 2
  done;
  let gibbs_prob v =
    asn.(v) <- 1;
    let pt = ref (prow v asn) in
    List.iter (fun c ->
      let p = prow c asn in
      pt := !pt *. (if asn.(c) = 1 then p else 1.0 -. p)
    ) children.(v);
    asn.(v) <- 0;
    let pv = ref (1.0 -. prow v asn) in
    List.iter (fun c ->
      let p = prow c asn in
      pv := !pv *. (if asn.(c) = 1 then p else 1.0 -. p)
    ) children.(v);
    if !pt +. !pv = 0.0 then (if asn.(v) = 1 then 1.0 else 0.0)
    else !pt /. (!pt +. !pv)
  in
  for _ = 0 to 9 do
    for v = 0 to n - 1 do
      if evid.(v) < 0 then
        asn.(v) <- if LCG.next_double rng < gibbs_prob v then 1 else 0
    done
  done;
  let qt = ref 0 in
  for _ = 0 to s_samples - 1 do
    for v = 0 to n - 1 do
      if evid.(v) < 0 then
        asn.(v) <- if LCG.next_double rng < gibbs_prob v then 1 else 0
    done;
    if asn.(q) = 1 then incr qt
  done;
  Printf.printf "GIBBS %d %d\n" !qt s_samples

let () =
  try
    let t = Scanf.scanf " %d" (fun x -> x) in
    for _ = 0 to t - 1 do
      solve ()
    done
  with End_of_file -> ()
