(* Gaussian kernel density estimation (Think Stats 2e ch 6). *)

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
      let xs = List.init n (fun i -> float_of_string arr.(1 + i)) in
      let h = float_of_string arr.(1 + n) in
      let m = int_of_string arr.(2 + n) in
      let qs = List.init m (fun i -> float_of_string arr.(3 + n + i)) in
      let coef = 1.0 /. (h *. sqrt (2.0 *. Float.pi)) in
      let denom = 2.0 *. h *. h in
      let out =
        List.map
          (fun q ->
            let s =
              List.fold_left
                (fun a x ->
                  let d = q -. x in
                  a +. exp (-.(d *. d /. denom)))
                0.0 xs
            in
            Printf.sprintf "%.6f" (coef *. s /. float_of_int n))
          qs
      in
      print_string (String.concat "\n" out)
