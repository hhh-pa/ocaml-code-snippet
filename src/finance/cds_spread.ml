(* CDS hazard bootstrap + par spread (FE ch 16). *)

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

let () =
  match split_ws (read_all ()) with
  | [] -> ()
  | toks ->
      let arr = Array.of_list toks in
      let recov = float_of_string arr.(0) in
      let r = float_of_string arr.(1) in
      let n = int_of_string arr.(2) in
      let ts = Array.init n (fun i -> float_of_string arr.(3 + (2 * i))) in
      let ds = Array.init n (fun i -> float_of_string arr.(4 + (2 * i))) in
      let hazards = ref [] in
      let prev_t = ref 0.0 in
      let prev_s = ref 1.0 in
      let prot = ref 0.0 in
      let prem = ref 0.0 in
      for i = 0 to n - 1 do
        let df = exp (-.r *. ts.(i)) in
        let s = ds.(i) /. df in
        let lam = -.log (s /. !prev_s) /. (ts.(i) -. !prev_t) in
        hazards := Printf.sprintf "%.6f" lam :: !hazards;
        prot := !prot +. (df *. (!prev_s -. s));
        prem := !prem +. ((ts.(i) -. !prev_t) *. df *. s);
        prev_t := ts.(i);
        prev_s := s
      done;
      prot := !prot *. (1.0 -. recov);
      let spread = !prot /. !prem in
      Printf.printf "HAZARDS %s\nSPREAD %.6f\nSPREAD_BPS %.6f"
        (String.concat " " (List.rev !hazards))
        spread (spread *. 10000.0)
