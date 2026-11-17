(* Sorting unsigned permutations by reversals (first heuristic, 4-approx). *)

let breakpoints p =
  let n = Array.length p in
  let bps =
    ref ((if p.(0) <> 1 then 1 else 0) + if p.(n - 1) <> n then 1 else 0)
  in
  for i = 0 to n - 2 do
    if abs (p.(i) - p.(i + 1)) <> 1 then incr bps
  done;
  !bps

let has_decreasing p =
  let n = Array.length p in
  let l = ref 0 and found = ref false in
  for i = 0 to n - 2 do
    if abs (p.(i) - p.(i + 1)) <> 1 then begin
      if !l = i then found := true
      else begin
        let dec = ref true in
        for j = !l to i - 1 do
          if p.(j) < p.(j + 1) then dec := false
        done;
        if !dec then found := true
      end;
      l := i + 1
    end
  done;
  if !l = n - 1 then true
  else
    begin if !found then true
    else begin
      let dec = ref true in
      for j = !l to n - 2 do
        if p.(j) < p.(j + 1) then dec := false
      done;
      !dec
    end
    end

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
  | _ :: rest ->
      let p = ref (Array.of_list (List.map int_of_string rest)) in
      let n = Array.length !p in
      let ops = ref [] in
      while breakpoints !p > 0 do
        let dec = has_decreasing !p
        and cur = breakpoints !p
        and found = ref false in
        for l = 0 to n - 1 do
          for r = l + 1 to n - 1 do
            if not !found then begin
              let q = Array.copy !p in
              let a = ref l and b = ref r in
              while !a < !b do
                let t = q.(!a) in
                q.(!a) <- q.(!b);
                q.(!b) <- t;
                incr a;
                decr b
              done;
              let bq = breakpoints q in
              if
                (dec && bq < cur) || ((not dec) && bq <= cur && has_decreasing q)
              then begin
                p := q;
                ops := (l + 1, r + 1) :: !ops;
                found := true
              end
            end
          done
        done
      done;
      let parts =
        List.rev_map (fun (l, r) -> Printf.sprintf "%d %d" l r) !ops
      in
      Printf.printf "%d %s" (List.length !ops) (String.concat " " parts)
  | _ -> ()
