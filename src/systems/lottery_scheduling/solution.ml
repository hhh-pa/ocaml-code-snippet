(* Lottery scheduling (MOS 2.4, Waldspurger-Weihl). *)

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

let lcg_mod = 1 lsl 31

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
      let e = int_of_string (next ()) in
      let rng = ref (int_of_string (next ())) in
      let tix = Array.make n 0 in
      let total = ref 0 in
      let out = Buffer.create 256 in
      for _ = 1 to e do
        let op = next () in
        if op = "DRAW" then (
          if !total = 0 then Buffer.add_string out "EMPTY\n"
          else
            let lim = lcg_mod / !total * !total in
            let r = ref !rng in
            r := ((1103515245 * !r) + 12345) mod lcg_mod;
            while !r >= lim do
              r := ((1103515245 * !r) + 12345) mod lcg_mod
            done;
            rng := !r;
            let w = !r mod !total in
            let acc = ref 0 and p = ref 0 in
            while !p < n && w >= !acc + tix.(!p) do
              acc := !acc + tix.(!p);
              incr p
            done;
            Buffer.add_string out (Printf.sprintf "WINNER %d\n" !p))
        else if op = "SET" then (
          let p = int_of_string (next ()) in
          let tv = int_of_string (next ()) in
          total := !total + tv - tix.(p);
          tix.(p) <- tv;
          Buffer.add_string out (Printf.sprintf "OK %d\n" !total))
        else
          let a = int_of_string (next ()) in
          let b = int_of_string (next ()) in
          let tv = int_of_string (next ()) in
          if tix.(a) < tv then Buffer.add_string out "REJECTED\n"
          else (
            tix.(a) <- tix.(a) - tv;
            tix.(b) <- tix.(b) + tv;
            Buffer.add_string out (Printf.sprintf "OK %d\n" !total))
      done;
      print_string (Buffer.contents out)
