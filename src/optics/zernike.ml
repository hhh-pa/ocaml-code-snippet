(* Zernike fit + RMS (geom ch 13). *)

let read_all () =
  let buf = Buffer.create 256 in
  (try
     while true do
       Buffer.add_char buf (input_char stdin)
     done
   with End_of_file -> ());
  Buffer.contents buf

let is_space = function ' ' | '\t' | '\n' | '\r' -> true | _ -> false

let split_ws s =
  let toks = ref [] in
  let cur = Buffer.create 16 in
  let flush () =
    if Buffer.length cur > 0 then begin
      toks := Buffer.contents cur :: !toks;
      Buffer.clear cur
    end
  in
  String.iter
    (fun c -> if is_space c then flush () else Buffer.add_char cur c)
    s;
  flush ();
  List.rev !toks

let snap x = if abs_float x < 5e-7 then 0.0 else x

let fact n =
  let f = ref 1 in
  for i = 2 to n do
    f := !f * i
  done;
  !f

let nm_of j =
  let n = ref 0 in
  while (!n + 1) * (!n + 2) / 2 < j do
    incr n
  done;
  let n = !n in
  let r = j - (n * (n + 1) / 2) - 1 in
  let m =
    if n mod 2 = 0 then if r = 0 then 0 else if r mod 2 = 1 then r + 1 else -r
    else if r mod 2 = 0 then r + 1
    else -r
  in
  (n, m)

let radial n m ro =
  let am = abs m in
  let tot = ref 0.0 in
  for s = 0 to (n - am) / 2 do
    let num = fact (n - s) in
    let den = fact s * fact (((n + am) / 2) - s) * fact (((n - am) / 2) - s) in
    let pw = ref 1.0 in
    for _ = 1 to n - (2 * s) do
      pw := !pw *. ro
    done;
    let term = float_of_int num /. float_of_int den *. !pw in
    tot := !tot +. if s mod 2 = 0 then term else -.term
  done;
  !tot

let zernike j ro th =
  let n, m = nm_of j in
  let r = radial n m ro in
  if m = 0 then sqrt (float_of_int (n + 1)) *. r
  else if m > 0 then
    sqrt (float_of_int (2 * (n + 1))) *. r *. cos (float_of_int m *. th)
  else sqrt (float_of_int (2 * (n + 1))) *. r *. sin (float_of_int (-m) *. th)

let gauss_solve mat vec beta =
  let k = Array.length mat in
  let singular = ref false in
  let s = ref 0 in
  while !s < k && not !singular do
    let piv = ref !s in
    for r = !s + 1 to k - 1 do
      if abs_float mat.(r).(!s) > abs_float mat.(!piv).(!s) then piv := r
    done;
    let tmp = mat.(!s) in
    mat.(!s) <- mat.(!piv);
    mat.(!piv) <- tmp;
    let tv = vec.(!s) in
    vec.(!s) <- vec.(!piv);
    vec.(!piv) <- tv;
    if mat.(!s).(!s) = 0.0 then singular := true
    else begin
      for r = !s + 1 to k - 1 do
        let factor = mat.(r).(!s) /. mat.(!s).(!s) in
        for c = !s to k - 1 do
          mat.(r).(c) <- mat.(r).(c) -. (factor *. mat.(!s).(c))
        done;
        vec.(r) <- vec.(r) -. (factor *. vec.(!s))
      done;
      incr s
    end
  done;
  if !singular then false
  else begin
    for s = k - 1 downto 0 do
      let tot = ref vec.(s) in
      for c = s + 1 to k - 1 do
        tot := !tot -. (mat.(s).(c) *. beta.(c))
      done;
      beta.(s) <- !tot /. mat.(s).(s)
    done;
    true
  end

let () =
  match split_ws (read_all ()) with
  | [] -> ()
  | toks ->
      let arr = Array.of_list toks in
      let jn = int_of_string arr.(0) in
      let n = int_of_string arr.(1) in
      let ros = Array.init n (fun i -> float_of_string arr.(2 + (3 * i))) in
      let ths = Array.init n (fun i -> float_of_string arr.(3 + (3 * i))) in
      let ws = Array.init n (fun i -> float_of_string arr.(4 + (3 * i))) in
      let cols =
        Array.init n (fun i ->
            Array.init jn (fun a -> zernike (a + 1) ros.(i) ths.(i)))
      in
      let mat = Array.make_matrix jn jn 0.0 in
      let vec = Array.make jn 0.0 in
      for i = 0 to n - 1 do
        for a = 0 to jn - 1 do
          for b = 0 to jn - 1 do
            mat.(a).(b) <- mat.(a).(b) +. (cols.(i).(a) *. cols.(i).(b))
          done;
          vec.(a) <- vec.(a) +. (cols.(i).(a) *. ws.(i))
        done
      done;
      let beta = Array.make jn 0.0 in
      if not (gauss_solve mat vec beta) then print_string "SINGULAR"
      else
        let ssres = ref 0.0 in
        let ssdat = ref 0.0 in
        for i = 0 to n - 1 do
          let pred = ref 0.0 in
          for a = 0 to jn - 1 do
            pred := !pred +. (beta.(a) *. cols.(i).(a))
          done;
          let r = ws.(i) -. !pred in
          ssres := !ssres +. (r *. r);
          ssdat := !ssdat +. (ws.(i) *. ws.(i))
        done;
        let rms = sqrt (!ssres /. float_of_int n) in
        let rmsd = sqrt (!ssdat /. float_of_int n) in
        let coeffs =
          Array.to_list
            (Array.map (fun b -> Printf.sprintf "%.6f" (snap b)) beta)
        in
        Printf.printf "COEFFS %s\nRMS %.6f\nRMSDATA %.6f"
          (String.concat " " coeffs) (snap rms) (snap rmsd)
