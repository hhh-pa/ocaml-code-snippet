(* Variance-swap replication strike (FE ch 14). *)

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
      let fwd = float_of_string arr.(2) in
      let m = int_of_string arr.(3) in
      let ks = Array.init m (fun i -> float_of_string arr.(4 + (3 * i))) in
      let calls = Array.init m (fun i -> float_of_string arr.(5 + (3 * i))) in
      let puts = Array.init m (fun i -> float_of_string arr.(6 + (3 * i))) in
      let k0 = ref ks.(0) in
      for i = 0 to m - 1 do
        if ks.(i) <= fwd then k0 := ks.(i)
      done;
      let acc = ref 0.0 in
      for i = 0 to m - 1 do
        let dk =
          if i = 0 then ks.(1) -. ks.(0)
          else if i = m - 1 then ks.(m - 1) -. ks.(m - 2)
          else (ks.(i + 1) -. ks.(i - 1)) /. 2.0
        in
        let q =
          if ks.(i) < !k0 then puts.(i)
          else if ks.(i) > !k0 then calls.(i)
          else (calls.(i) +. puts.(i)) /. 2.0
        in
        acc := !acc +. (dk /. (ks.(i) *. ks.(i)) *. q)
      done;
      let adj = (fwd /. !k0) -. 1.0 in
      let kvar = (2.0 *. exp (r *. t) /. t *. !acc) -. (adj *. adj /. t) in
      let vol = sqrt (max kvar 0.0) in
      Printf.printf "VARSTRIKE %.6f\nVOLSTRIKE %.6f" kvar vol
