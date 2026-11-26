(* Fabry-Perot etalon transmission (wave ch 3). *)

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

let () =
  match split_ws (read_all ()) with
  | [] -> ()
  | toks ->
      let arr = Array.of_list toks in
      let r = float_of_string arr.(0) in
      let n = float_of_string arr.(1) in
      let d = float_of_string arr.(2) in
      let theta = float_of_string arr.(3) in
      let m = int_of_string arr.(4) in
      let lams = Array.init m (fun i -> float_of_string arr.(5 + i)) in
      let fcoef = 4.0 *. r /. ((1.0 -. r) *. (1.0 -. r)) in
      let finesse = Float.pi *. sqrt r /. (1.0 -. r) in
      let trad = theta *. Float.pi /. 180.0 in
      let cost = cos trad in
      let out = Buffer.create 64 in
      Buffer.add_string out (Printf.sprintf "FINESSE %.6f" (snap finesse));
      for i = 0 to m - 1 do
        let k = 4.0 *. Float.pi /. lams.(i) in
        let de = k *. n *. d *. cost in
        let s = sin (de /. 2.0) in
        let t = 1.0 /. (1.0 +. (fcoef *. s *. s)) in
        Buffer.add_string out (Printf.sprintf "\nT %.6f" (snap t))
      done;
      print_string (Buffer.contents out)
