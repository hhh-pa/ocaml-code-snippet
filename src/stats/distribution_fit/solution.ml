(* Distribution fitting (Think Stats 2e ch 5). *)

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
      let nb = float_of_int (int_of_string arr.(1)) in
      let xs =
        List.init n (fun i -> float_of_int (int_of_string arr.(2 + i)))
      in
      let mean = List.fold_left ( +. ) 0.0 xs /. float_of_int n in
      let var =
        List.fold_left
          (fun a x ->
            let d = x -. mean in
            a +. (d *. d))
          0.0 xs
        /. float_of_int (n - 1)
      in
      let logs = List.map log10 xs in
      let lmean = List.fold_left ( +. ) 0.0 logs /. float_of_int n in
      let lvar =
        List.fold_left
          (fun a x ->
            let d = x -. lmean in
            a +. (d *. d))
          0.0 logs
        /. float_of_int (n - 1)
      in
      Printf.printf
        "BINOMIAL %.6f\n\
         POISSON %.6f\n\
         EXPONENTIAL %.6f\n\
         NORMAL %.6f %.6f\n\
         LOGNORMAL %.6f %.6f"
        (mean /. nb) mean (1.0 /. mean) mean (sqrt var) lmean (sqrt lvar)
