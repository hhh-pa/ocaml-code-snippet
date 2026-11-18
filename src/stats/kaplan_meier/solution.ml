(* Kaplan-Meier survival analysis (Think Stats 2e ch 13). *)

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

let remaining taus survs query =
  let m = List.length taus in
  if m = 0 || query >= List.nth taus (m - 1) then 0.0
  else
    let sq = ref 1.0 in
    List.iter2 (fun tau ss -> if tau <= query then sq := ss) taus survs;
    if !sq = 0.0 then 0.0
    else
      let area = ref 0.0 in
      let prev_t = ref 0 in
      let prev_s = ref 1.0 in
      List.iter2
        (fun tau ss ->
          (if tau > query then
             let lo = if !prev_t > query then !prev_t else query in
             area := !area +. (!prev_s *. float_of_int (tau - lo)));
          prev_t := tau;
          prev_s := ss)
        taus survs;
      !area /. !sq

let () =
  match split_ws (read_all ()) with
  | [] -> ()
  | toks ->
      let arr = Array.of_list toks in
      let n = int_of_string arr.(0) in
      let times = Array.init n (fun i -> int_of_string arr.(1 + (2 * i))) in
      let events = Array.init n (fun i -> int_of_string arr.(2 + (2 * i))) in
      let q = int_of_string arr.(1 + (2 * n)) in
      let queries =
        List.init q (fun i -> int_of_string arr.(2 + (2 * n) + i))
      in
      let eset = Hashtbl.create 16 in
      for i = 0 to n - 1 do
        if events.(i) = 1 then Hashtbl.replace eset times.(i) true
      done;
      let taus =
        List.sort Int.compare (Hashtbl.fold (fun t _ acc -> t :: acc) eset [])
      in
      let survs = ref [] in
      let hazards = ref [] in
      let s = ref 1.0 in
      List.iter
        (fun tau ->
          let d = ref 0 in
          let nr = ref 0 in
          for i = 0 to n - 1 do
            if times.(i) = tau && events.(i) = 1 then incr d;
            if times.(i) >= tau then incr nr
          done;
          s := !s *. (float_of_int (!nr - !d) /. float_of_int !nr);
          survs := !s :: !survs;
          hazards := (float_of_int !d /. float_of_int !nr) :: !hazards)
        taus;
      let survs = List.rev !survs in
      let hazards = List.rev !hazards in
      let out = Buffer.create 64 in
      let line str =
        if Buffer.length out > 0 then Buffer.add_char out '\n';
        Buffer.add_string out str
      in
      List.iter2
        (fun (tau, ss) hz ->
          line (Printf.sprintf "SURV %d %.6f %.6f" tau ss hz))
        (List.combine taus survs) hazards;
      List.iter
        (fun query ->
          line (Printf.sprintf "REM %d %.6f" query (remaining taus survs query)))
        queries;
      print_string (Buffer.contents out)
