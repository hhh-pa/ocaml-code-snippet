(* Standard error and bootstrap CIs (Think Stats 2e ch 8). *)

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
      let xs = Array.init n (fun i -> int_of_string arr.(1 + i)) in
      let b = int_of_string arr.(1 + n) in
      let rng = ref (int_of_string arr.(2 + n)) in
      let modulus = 1 lsl 31 in
      let sum = Array.fold_left (fun a x -> a +. float_of_int x) 0.0 xs in
      let mean = sum /. float_of_int n in
      let var =
        Array.fold_left
          (fun a x ->
            let d = float_of_int x -. mean in
            a +. (d *. d))
          0.0 xs
        /. float_of_int (n - 1)
      in
      let se = sqrt var /. sqrt (float_of_int n) in
      let lo = mean -. (1.96 *. se) in
      let hi = mean +. (1.96 *. se) in
      let means = ref [] in
      for _ = 1 to b do
        let draw = ref 0 in
        for _ = 1 to n do
          rng := ((1103515245 * !rng) + 12345) mod modulus;
          draw := !draw + xs.(!rng mod n)
        done;
        means := (float_of_int !draw /. float_of_int n) :: !means
      done;
      let srt = Array.of_list (List.sort Float.compare !means) in
      let rank p =
        min b (max 1 (half_even (p *. float_of_int (b + 1) /. 100.0)))
      in
      let blo = srt.(rank 2.5 - 1) in
      let bhi = srt.(rank 97.5 - 1) in
      Printf.printf "SE %.6f\nCI %.6f %.6f\nBOOT %.6f %.6f" se lo hi blo bhi
