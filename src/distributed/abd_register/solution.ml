(* ABD multi-writer atomic register emulation (Lynch 17.1.3 + MWMR). *)

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

let tag_gt (s1, w1) (s2, w2) = s1 > s2 || (s1 = s2 && w1 > w2)

let () =
  match split_ws (read_all ()) with
  | [] -> ()
  | toks ->
      let t = Array.of_list toks in
      let pos = ref 0 in
      let next () =
        let v = t.(!pos) in
        incr pos;
        v
      in
      let n = int_of_string (next ()) in
      let m = int_of_string (next ()) in
      let k = int_of_string (next ()) in
      let crashed = Array.make n false in
      for _ = 1 to k do
        crashed.(int_of_string (next ())) <- true
      done;
      let correct = ref [] in
      for r = 0 to n - 1 do
        if not crashed.(r) then correct := r :: !correct
      done;
      let correct = List.rev !correct in
      let quorum = (n / 2) + 1 in
      let live p = List.length correct >= quorum && not crashed.(p) in
      let vals = Array.make n 0L in
      let tag = Array.make n (0, 0) in
      let out = Buffer.create 256 in
      for _ = 1 to m do
        let op = next () in
        let p = int_of_string (next ()) in
        if op = "W" then (
          let v = Int64.of_string (next ()) in
          if not (live p) then Buffer.add_string out "BLOCKED\n"
          else
            let best = ref (0, 0) in
            List.iter
              (fun r -> if tag_gt tag.(r) !best then best := tag.(r))
              correct;
            let nt = (fst !best + 1, p) in
            List.iter
              (fun r ->
                if tag_gt nt tag.(r) then begin
                  tag.(r) <- nt;
                  vals.(r) <- v
                end)
              correct;
            Buffer.add_string out (Printf.sprintf "ACK %d %d\n" (fst nt) p))
        else if not (live p) then Buffer.add_string out "BLOCKED\n"
        else
          let best = ref (0, 0) in
          let bv = ref 0L in
          List.iter
            (fun r ->
              if tag_gt tag.(r) !best then begin
                best := tag.(r);
                bv := vals.(r)
              end)
            correct;
          List.iter
            (fun r ->
              if tag_gt !best tag.(r) then begin
                tag.(r) <- !best;
                vals.(r) <- !bv
              end)
            correct;
          Buffer.add_string out
            (Printf.sprintf "VAL %Ld %d %d\n" !bv (fst !best) (snd !best))
      done;
      print_string (Buffer.contents out)
