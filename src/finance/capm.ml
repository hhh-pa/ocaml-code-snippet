(* CAPM beta + expected return (Gestion ch 2). *)

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
      let n = int_of_string arr.(0) in
      let ris = Array.init n (fun i -> float_of_string arr.(1 + (2 * i))) in
      let rms = Array.init n (fun i -> float_of_string arr.(2 + (2 * i))) in
      let rf = float_of_string arr.(1 + (2 * n)) in
      let mi = Array.fold_left ( +. ) 0.0 ris /. float_of_int n in
      let mm = Array.fold_left ( +. ) 0.0 rms /. float_of_int n in
      let c = ref 0.0 in
      let vm = ref 0.0 in
      for i = 0 to n - 1 do
        let di = ris.(i) -. mi in
        let dm = rms.(i) -. mm in
        c := !c +. (di *. dm);
        vm := !vm +. (dm *. dm)
      done;
      let beta = if !vm = 0.0 then 0.0 else !c /. !vm in
      let exp = rf +. (beta *. (mm -. rf)) in
      Printf.printf "BETA %.6f\nEXP %.6f" beta exp
