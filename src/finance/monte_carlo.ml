(* Monte Carlo GBM option pricing (FRM ch 4). *)

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

let lcg_a = 6364136223846793005L
let lcg_c = 1442695040888963407L
let den = 9007199254740992.0

let () =
  match split_ws (read_all ()) with
  | [] -> ()
  | toks ->
      let arr = Array.of_list toks in
      let s0 = float_of_string arr.(0) in
      let k = float_of_string arr.(1) in
      let t = float_of_string arr.(2) in
      let r = float_of_string arr.(3) in
      let sg = float_of_string arr.(4) in
      let m = int_of_string arr.(5) in
      let st = ref (Int64.of_string arr.(6)) in
      let step () =
        (st := Int64.(add (mul !st lcg_a) lcg_c));
        Int64.(to_float (add (shift_right_logical !st 11) 1L)) /. den
      in
      let zs = ref [] in
      while List.length !zs < m do
        let u1 = step () in
        let u2 = step () in
        let rad = sqrt (-2.0 *. log u1) in
        let ang = 2.0 *. Float.pi *. u2 in
        zs := (rad *. cos ang) :: !zs;
        if List.length !zs < m then zs := (rad *. sin ang) :: !zs
      done;
      let zs = List.rev !zs in
      let drift = (r -. (sg *. sg /. 2.0)) *. t in
      let vol = sg *. sqrt t in
      let tot_c = ref 0.0 in
      let tot_p = ref 0.0 in
      List.iter
        (fun z ->
          let sp = s0 *. exp (drift +. (vol *. z)) in
          let sm = s0 *. exp (drift +. (vol *. -.z)) in
          tot_c := !tot_c +. max (sp -. k) 0.0 +. max (sm -. k) 0.0;
          tot_p := !tot_p +. max (k -. sp) 0.0 +. max (k -. sm) 0.0)
        zs;
      let disc = exp (-.r *. t) in
      let call = disc *. (!tot_c /. float_of_int (2 * m)) in
      let put = disc *. (!tot_p /. float_of_int (2 * m)) in
      Printf.printf "CALL %.6f\nPUT %.6f" call put
