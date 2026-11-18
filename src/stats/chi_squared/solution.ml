(* Chi-squared tests (Think Stats 2e ch 14). *)

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
      let stat, dof =
        if arr.(0) = "GOF" then (
          let k = int_of_string arr.(1) in
          let obs = Array.init k (fun i -> int_of_string arr.(2 + i)) in
          let probs = Array.init k (fun i -> float_of_string arr.(2 + k + i)) in
          let n = Array.fold_left ( + ) 0 obs in
          let stat = ref 0.0 in
          for i = 0 to k - 1 do
            let e = float_of_int n *. probs.(i) in
            let d = float_of_int obs.(i) -. e in
            stat := !stat +. (d *. d /. e)
          done;
          (!stat, k - 1))
        else
          let r = int_of_string arr.(1) in
          let c = int_of_string arr.(2) in
          let cells = Array.init (r * c) (fun i -> int_of_string arr.(3 + i)) in
          let total = Array.fold_left ( + ) 0 cells in
          let rows = Array.make r 0 in
          let cols = Array.make c 0 in
          for i = 0 to r - 1 do
            for j = 0 to c - 1 do
              rows.(i) <- rows.(i) + cells.((i * c) + j);
              cols.(j) <- cols.(j) + cells.((i * c) + j)
            done
          done;
          let stat = ref 0.0 in
          for i = 0 to r - 1 do
            for j = 0 to c - 1 do
              let e =
                float_of_int (rows.(i) * cols.(j)) /. float_of_int total
              in
              if e <> 0.0 then
                let d = float_of_int cells.((i * c) + j) -. e in
                stat := !stat +. (d *. d /. e)
            done
          done;
          (!stat, (r - 1) * (c - 1))
      in
      Printf.printf "STAT %.6f\nDOF %d" stat dof
