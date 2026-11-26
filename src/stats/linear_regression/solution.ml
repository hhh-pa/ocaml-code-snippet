(* Least squares fit + R^2 (Think Stats 2e ch 10). *)

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
      let xs = Array.init n (fun i -> int_of_string arr.(1 + (2 * i))) in
      let ys = Array.init n (fun i -> int_of_string arr.(2 + (2 * i))) in
      let sx = Array.fold_left (fun a x -> a +. float_of_int x) 0.0 xs in
      let sy = Array.fold_left (fun a y -> a +. float_of_int y) 0.0 ys in
      let mx = sx /. float_of_int n in
      let my = sy /. float_of_int n in
      let c = ref 0.0 in
      let vx = ref 0.0 in
      for i = 0 to n - 1 do
        let dx = float_of_int xs.(i) -. mx in
        let dy = float_of_int ys.(i) -. my in
        c := !c +. (dx *. dy);
        vx := !vx +. (dx *. dx)
      done;
      let slope, inter, r2 =
        if !vx = 0.0 then (0.0, my, 0.0)
        else
          let slope = !c /. !vx in
          let inter = my -. (slope *. mx) in
          let ssres = ref 0.0 in
          let sstot = ref 0.0 in
          for i = 0 to n - 1 do
            let pred = (slope *. float_of_int xs.(i)) +. inter in
            let r = float_of_int ys.(i) -. pred in
            ssres := !ssres +. (r *. r);
            let d = float_of_int ys.(i) -. my in
            sstot := !sstot +. (d *. d)
          done;
          let r2 = if !sstot = 0.0 then 0.0 else 1.0 -. (!ssres /. !sstot) in
          (slope, inter, r2)
      in
      Printf.printf "SLOPE %.6f\nINTER %.6f\nRSQ %.6f" slope inter r2
