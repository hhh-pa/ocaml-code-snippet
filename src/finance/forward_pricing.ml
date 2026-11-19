(* Cost-of-carry forwards + FRA valuation (Gestion ch 10, FE ch 3-4). *)

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
      let arr = Array.of_list (List.map float_of_string toks) in
      let eqf = arr.(0) *. exp ((arr.(1) -. arr.(2)) *. arr.(3)) in
      let fxf = arr.(4) *. exp ((arr.(5) -. arr.(6)) *. arr.(7)) in
      let commf =
        arr.(8) *. exp ((arr.(9) +. arr.(10) -. arr.(11)) *. arr.(12))
      in
      let frav = arr.(13) *. arr.(14) *. (arr.(15) -. arr.(16)) *. arr.(17) in
      Printf.printf "EQF %.6f\nFXF %.6f\nCOMMF %.6f\nFRAV %.6f" eqf fxf commf
        frav
