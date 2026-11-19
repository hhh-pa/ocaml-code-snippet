(* RED gateway simulation (Tanenbaum 5.4 + Floyd-Jacobson 1993). *)

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

let scale = 1000
let lcg_mod = 1 lsl 31

let () =
  match split_ws (read_all ()) with
  | [] -> ()
  | toks ->
      let nums = Array.of_list (List.map int_of_string toks) in
      let wqn = nums.(0)
      and wqd = nums.(1)
      and mn = nums.(2)
      and mx = nums.(3)
      and pn = nums.(4)
      and pd = nums.(5)
      and cap = nums.(6)
      and seed = nums.(7) in
      let m = nums.(8) in
      let q = ref 0
      and avg = ref 0
      and count = ref 0
      and rng = ref seed
      and now = ref 0 in
      let out = Buffer.create 256 in
      for i = 0 to m - 1 do
        let t = nums.(9 + i) in
        while !now <= t do
          if !q > 0 then decr q else avg := !avg * (wqd - wqn) / wqd;
          incr now
        done;
        avg := ((!avg * (wqd - wqn)) + (wqn * !q * scale)) / wqd;
        if !q = cap then begin
          Buffer.add_string out "DROP\n";
          count := 0
        end
        else if !avg < mn * scale then begin
          Buffer.add_string out "ENQUEUE\n";
          incr q;
          count := 0
        end
        else if !avg >= mx * scale then begin
          Buffer.add_string out "DROP\n";
          count := 0
        end
        else begin
          incr count;
          let a = !avg - (mn * scale) and r = (mx - mn) * scale in
          if !count * pn * a >= pd * r then begin
            Buffer.add_string out "DROP\n";
            count := 0
          end
          else begin
            rng := ((1103515245 * !rng) + 12345) mod lcg_mod;
            if !rng * ((pd * r) - (!count * pn * a)) < lcg_mod * pn * a then begin
              Buffer.add_string out "DROP\n";
              count := 0
            end
            else begin
              Buffer.add_string out "ENQUEUE\n";
              incr q
            end
          end
        end
      done;
      Printf.bprintf out "FINAL %d %d %d" !q !avg !count;
      print_string (Buffer.contents out)
