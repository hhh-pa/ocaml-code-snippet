(* PMF/CDF queries (Think Stats 2e ch 3-4). *)

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

let half_even r =
  let q = int_of_float (Float.floor r) in
  let f = r -. float_of_int q in
  if f > 0.5 then q + 1
  else if f < 0.5 then q
  else if q mod 2 = 0 then q
  else q + 1

let () =
  match split_ws (read_all ()) with
  | [] -> ()
  | toks ->
      let arr = Array.of_list toks in
      let n = int_of_string arr.(0) in
      let xs = List.init n (fun i -> int_of_string arr.(1 + i)) in
      let srt = Array.of_list (List.sort Int.compare xs) in
      let i0 = 1 + n in
      let q = int_of_string arr.(i0) in
      let out = Buffer.create 64 in
      let i = ref (i0 + 1) in
      for _ = 1 to q do
        let op = arr.(!i) in
        if Buffer.length out > 0 then Buffer.add_char out '\n';
        if op = "PCTL" then begin
          let p = float_of_string arr.(!i + 1) in
          i := !i + 2;
          let r = p *. float_of_int (n + 1) /. 100.0 in
          let rank = min n (max 1 (half_even r)) in
          Buffer.add_string out (string_of_int srt.(rank - 1))
        end
        else begin
          let v = int_of_string arr.(!i + 1) in
          i := !i + 2;
          let cnt =
            List.fold_left
              (fun a x ->
                if (op = "PMF" && x = v) || (op <> "PMF" && x <= v) then a + 1
                else a)
              0 xs
          in
          let value =
            if op = "PMF" || op = "CDF" then float_of_int cnt /. float_of_int n
            else float_of_int (100 * cnt) /. float_of_int n
          in
          Buffer.add_string out (Printf.sprintf "%.6f" value)
        end
      done;
      print_string (Buffer.contents out)
