(* Breeden-Litzenberger density + CDF (FE ch 15). *)

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
      let t = float_of_string arr.(0) in
      let r = float_of_string arr.(1) in
      let m = int_of_string arr.(2) in
      let ks = Array.init m (fun i -> float_of_string arr.(3 + (2 * i))) in
      let calls = Array.init m (fun i -> float_of_string arr.(4 + (2 * i))) in
      let h = ks.(1) -. ks.(0) in
      let disc = exp (r *. t) in
      let out = Buffer.create 64 in
      for i = 1 to m - 2 do
        let d2 =
          (calls.(i + 1) -. (2.0 *. calls.(i)) +. calls.(i - 1)) /. (h *. h)
        in
        let f = disc *. d2 in
        let d1 = (calls.(i + 1) -. calls.(i - 1)) /. (2.0 *. h) in
        let cdf = 1.0 +. (disc *. d1) in
        if Buffer.length out > 0 then Buffer.add_char out '\n';
        Buffer.add_string out (Printf.sprintf "%.6f %.6f %.6f" ks.(i) f cdf)
      done;
      print_string (Buffer.contents out)
