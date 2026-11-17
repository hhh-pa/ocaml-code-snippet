(* Tree diameter in edges (two-pass farthest-node search). *)

let td_bfs adj s =
  let n = Array.length adj in
  let dist = Array.make n (-1) in
  let q = Queue.create () in
  Queue.add s q;
  dist.(s) <- 0;
  while not (Queue.is_empty q) do
    let u = Queue.take q in
    List.iter
      (fun w ->
        if dist.(w) < 0 then begin
          dist.(w) <- dist.(u) + 1;
          Queue.add w q
        end)
      adj.(u)
  done;
  dist

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
      let nums = Array.of_list (List.map int_of_string toks) in
      let n = nums.(0) in
      let adj = Array.make n [] in
      for i = 0 to n - 2 do
        let u = nums.(1 + (2 * i)) and v = nums.(2 + (2 * i)) in
        adj.(u) <- v :: adj.(u);
        adj.(v) <- u :: adj.(v)
      done;
      if n = 1 then print_int 0
      else
        let d0 = td_bfs adj 0 in
        let a = ref 0 in
        for i = 1 to n - 1 do
          if d0.(i) > d0.(!a) then a := i
        done;
        let d1 = td_bfs adj !a in
        let best = ref 0 in
        Array.iter (fun d -> best := max !best d) d1;
        print_int !best
