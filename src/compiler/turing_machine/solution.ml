(* Bounded-step deterministic Turing machine simulation (Rosen 13). *)

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
      let t = Array.of_list toks in
      let accept = int_of_string t.(1)
      and reject = int_of_string t.(2)
      and bound = int_of_string t.(3) in
      let m = int_of_string t.(4) in
      let trans = Hashtbl.create 16 in
      for i = 0 to m - 1 do
        let b = 5 + (5 * i) in
        let q = int_of_string t.(b)
        and a = t.(b + 1).[0]
        and w = t.(b + 2).[0]
        and d = t.(b + 3).[0]
        and q2 = int_of_string t.(b + 4) in
        Hashtbl.add trans (q, a) (w, d, q2)
      done;
      let p = 5 + (5 * m) in
      let word = if p < Array.length t then t.(p) else "" in
      let tape = Hashtbl.create 16 in
      String.iteri (fun i c -> Hashtbl.add tape i c) word;
      let state = ref 0 and head = ref 0 and steps = ref 0 in
      let result = ref "TIMEOUT" in
      while !steps < bound && !result = "TIMEOUT" do
        let sym =
          match Hashtbl.find_opt tape !head with None -> '.' | Some c -> c
        in
        match Hashtbl.find_opt trans (!state, sym) with
        | None -> result := Printf.sprintf "REJECT %d" !steps
        | Some (w, d, q2) ->
            Hashtbl.replace tape !head w;
            if d = 'L' then decr head else if d = 'R' then incr head;
            state := q2;
            incr steps;
            if !state = accept then result := Printf.sprintf "ACCEPT %d" !steps
            else if !state = reject then
              result := Printf.sprintf "REJECT %d" !steps
      done;
      print_string !result
