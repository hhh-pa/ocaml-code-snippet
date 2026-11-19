(* Weighted fair queueing (Tanenbaum 5.4 finish-tag formula). *)

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

let rec wfq_gcd a b = if b = 0 then a else wfq_gcd b (a mod b)

let before_tag tags arrs flows i j =
  (tags.(i), arrs.(i), flows.(i), i) < (tags.(j), arrs.(j), flows.(j), j)

let () =
  match split_ws (read_all ()) with
  | [] -> ()
  | toks ->
      let nums = Array.of_list (List.map int_of_string toks) in
      let f = nums.(0) and m = nums.(1) in
      let weights = Array.init f (fun i -> nums.(2 + i)) in
      let flows = Array.init m (fun i -> nums.(2 + f + (3 * i)))
      and arrs = Array.init m (fun i -> nums.(3 + f + (3 * i)))
      and lens = Array.init m (fun i -> nums.(4 + f + (3 * i))) in
      let den = ref 1 in
      for i = 0 to f - 1 do
        den := !den * weights.(i) / wfq_gcd !den weights.(i)
      done;
      let last = Array.make f 0 in
      let tags = Array.make m 0 in
      for i = 0 to m - 1 do
        let base = max (arrs.(i) * !den) last.(flows.(i)) in
        tags.(i) <- base + (lens.(i) * (!den / weights.(flows.(i))));
        last.(flows.(i)) <- tags.(i)
      done;
      let done_ = Array.make m false in
      let order = ref [] in
      let now = ref 0 in
      while List.length !order < m do
        let best = ref (-1) in
        for i = 0 to m - 1 do
          if (not done_.(i)) && arrs.(i) <= !now then
            if !best < 0 || before_tag tags arrs flows i !best then best := i
        done;
        if !best < 0 then (
          let nxt = ref (-1) in
          for i = 0 to m - 1 do
            if (not done_.(i)) && (!nxt < 0 || arrs.(i) < !nxt) then
              nxt := arrs.(i)
          done;
          now := !nxt)
        else begin
          done_.(!best) <- true;
          order := !best :: !order;
          now := !now + lens.(!best)
        end
      done;
      let order = List.rev !order in
      let parts = List.map string_of_int order |> String.concat " " in
      let fracs =
        List.map
          (fun i ->
            let g = wfq_gcd tags.(i) !den in
            Printf.sprintf "%d/%d" (tags.(i) / g) (!den / g))
          order
        |> String.concat " "
      in
      Printf.printf "%s\n%s" parts fracs
