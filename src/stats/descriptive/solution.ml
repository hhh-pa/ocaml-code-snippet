(* Descriptive statistics + Cohen's d (Think Stats 2e ch 1-2). *)

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

let describe xs =
  let n = List.length xs in
  let sum = List.fold_left (fun a x -> a +. float_of_int x) 0.0 xs in
  let mean = sum /. float_of_int n in
  let tot =
    List.fold_left
      (fun a x ->
        let d = float_of_int x -. mean in
        a +. (d *. d))
      0.0 xs
  in
  let var = tot /. float_of_int (n - 1) in
  let srt = List.sort Int.compare xs in
  let med =
    if n mod 2 = 1 then float_of_int (List.nth srt (n / 2))
    else float_of_int (List.nth srt ((n / 2) - 1) + List.nth srt (n / 2)) /. 2.0
  in
  (n, mean, var, sqrt var, List.hd srt, List.nth srt (n - 1), med)

let () =
  match split_ws (read_all ()) with
  | [] -> ()
  | toks ->
      let nums = List.map int_of_string toks in
      let n1 = List.hd nums in
      let rest = List.tl nums in
      let g1, rest2 =
        ( List.filteri (fun i _ -> i < n1) rest,
          List.filteri (fun i _ -> i >= n1) rest )
      in
      let n2 = List.hd rest2 in
      let g2 = List.filteri (fun i _ -> i >= 1 && i <= n2) rest2 in
      let n1, m1, v1, s1, mn1, mx1, md1 = describe g1 in
      let n2, m2, v2, s2, mn2, mx2, md2 = describe g2 in
      let t1 = float_of_int n1 *. v1 in
      let t2 = float_of_int n2 *. v2 in
      let pv = (t1 +. t2) /. float_of_int (n1 + n2) in
      let d = (m1 -. m2) /. sqrt pv in
      Printf.printf
        "%d %.6f %.6f %.6f %d %d %.6f\n%d %.6f %.6f %.6f %d %d %.6f\nCOHEN %.6f"
        n1 m1 v1 s1 mn1 mx1 md1 n2 m2 v2 s2 mn2 mx2 md2 d
