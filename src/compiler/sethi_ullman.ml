type node = {
  leaf : bool;
  label : string;
  l : node option;
  r : node option;
  su : int;
}

let tokenize line =
  let toks = ref [] in
  let cur = Buffer.create 16 in
  let flush () =
    if Buffer.length cur > 0 then (
      toks := Buffer.contents cur :: !toks;
      Buffer.clear cur
    )
  in
  let len = String.length line in
  for i = 0 to len - 1 do
    let c = line.[i] in
    if c = ' ' || c = '\t' || c = '\r' then
      flush ()
    else if c = '(' || c = ')' then (
      flush ();
      toks := String.make 1 c :: !toks
    ) else
      Buffer.add_char cur c
  done;
  flush ();
  Array.of_list (List.rev !toks)

let parse_tree toks =
  let p = ref 0 in
  let rec parse_node () =
    if toks.(!p) = "(" then (
      incr p;
      let op = toks.(!p) in
      incr p;
      let left = parse_node () in
      let right = parse_node () in
      incr p; (* ')' *)
      let su = if left.su = right.su then left.su + 1 else max left.su right.su in
      { leaf = false; label = op; l = Some left; r = Some right; su }
    ) else (
      let label = toks.(!p) in
      incr p;
      { leaf = true; label; l = None; r = None; su = 1 }
    )
  in
  parse_node ()

let rec order n out =
  if n.leaf then
    out := n.label :: !out
  else (
    match n.l, n.r with
    | Some left, Some right ->
      if right.su > left.su then (
        order right out;
        order left out
      ) else (
        order left out;
        order right out
      );
      out := n.label :: !out
    | _ -> ()
  )

let () =
  let lines = ref [] in
  (try
     while true do
       lines := input_line stdin :: !lines
     done
   with End_of_file -> ());
  let lines = Array.of_list (List.rev !lines) in
  let num_lines = Array.length lines in
  let idx = ref 0 in
  while !idx < num_lines && String.trim lines.(!idx) = "" do
    incr idx
  done;
  if !idx < num_lines then (
    let t_cases = int_of_string (String.trim lines.(!idx)) in
    incr idx;
    for _ = 1 to t_cases do
      while !idx < num_lines && String.trim lines.(!idx) = "" do
        incr idx
      done;
      if !idx < num_lines then (
        let line = lines.(!idx) in
        incr idx;
        let toks = tokenize line in
        if Array.length toks > 0 then (
          let r = parse_tree toks in
          Printf.printf "SU %d\n" r.su;
          let o = ref [] in
          order r o;
          let o_list = List.rev !o in
          Printf.printf "ORDER %s\n" (String.concat " " o_list)
        )
      )
    done
  )
