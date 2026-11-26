(* DCF + Gordon DDM valuation (Gestion ch 7-9). *)

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
      let r = float_of_string arr.(0) in
      let g = float_of_string arr.(1) in
      let n = int_of_string arr.(2) in
      let fcfs = Array.init n (fun i -> float_of_string arr.(3 + i)) in
      let d1 = float_of_string arr.(3 + n) in
      let df = ref 1.0 in
      let pv = ref 0.0 in
      for i = 0 to n - 1 do
        df := !df *. (1.0 +. r);
        pv := !pv +. (fcfs.(i) /. !df)
      done;
      let tv = fcfs.(n - 1) *. (1.0 +. g) /. (r -. g) in
      let dcf = !pv +. (tv /. !df) in
      let ddm = d1 /. (r -. g) in
      Printf.printf "DCF %.6f\nDDM %.6f" dcf ddm
