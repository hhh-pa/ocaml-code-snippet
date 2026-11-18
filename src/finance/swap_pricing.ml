(* Par swap rate + PV01 (FE ch 5). *)

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
      let pv01 = ref 0.0 in
      let dfn = ref 0.0 in
      for i = 0 to n - 1 do
        let tau = float_of_string arr.(1 + (2 * i)) in
        let df = float_of_string arr.(2 + (2 * i)) in
        pv01 := !pv01 +. (tau *. df);
        dfn := df
      done;
      let rate = (1.0 -. !dfn) /. !pv01 in
      Printf.printf "SWAPRATE %.6f\nPV01 %.6f" rate !pv01
