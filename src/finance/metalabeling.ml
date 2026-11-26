(* Meta-labeling: correctness + fixed-tree size (AFML ch 3). *)

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

type node = Leaf of float | Split of int * float * int * int

let () =
  match split_ws (read_all ()) with
  | [] -> ()
  | toks ->
      let arr = Array.of_list toks in
      let e = int_of_string arr.(0) in
      let f = int_of_string arr.(1) in
      let sides =
        Array.init e (fun i -> int_of_string arr.(2 + ((2 + f) * i)))
      in
      let feats =
        Array.init e (fun i ->
            Array.init f (fun j -> float_of_string arr.(3 + ((2 + f) * i) + j)))
      in
      let labels =
        Array.init e (fun i -> int_of_string arr.(3 + f + ((2 + f) * i)))
      in
      let t0 = 2 + ((2 + f) * e) in
      let t = int_of_string arr.(t0) in
      let nodes = ref [] in
      let idx = ref (t0 + 1) in
      for _ = 1 to t do
        if arr.(!idx) = "LEAF" then begin
          nodes := Leaf (float_of_string arr.(!idx + 1)) :: !nodes;
          idx := !idx + 2
        end
        else begin
          nodes :=
            Split
              ( int_of_string arr.(!idx + 1),
                float_of_string arr.(!idx + 2),
                int_of_string arr.(!idx + 3),
                int_of_string arr.(!idx + 4) )
            :: !nodes;
          idx := !idx + 5
        end
      done;
      let nodes = Array.of_list (List.rev !nodes) in
      let out = Buffer.create 64 in
      let good = ref 0 in
      for i = 0 to e - 1 do
        let y = if sides.(i) = labels.(i) && labels.(i) <> 0 then 1 else 0 in
        good := !good + y;
        let node = ref 0 in
        let size = ref 0.0 in
        let stop = ref false in
        while not !stop do
          match nodes.(!node) with
          | Leaf v ->
              size := v;
              stop := true
          | Split (feat, thr, left, right) ->
              node := if feats.(i).(feat) <= thr then left else right
        done;
        if Buffer.length out > 0 then Buffer.add_char out '\n';
        Buffer.add_string out (Printf.sprintf "META %d SIZE %.6f" y !size)
      done;
      Buffer.add_string out
        (Printf.sprintf "\nACC %.6f" (float_of_int !good /. float_of_int e));
      print_string (Buffer.contents out)
