let read_all_tokens () =
  let content =
    let b = Buffer.create 4096 in
    let buf = Bytes.create 4096 in
    let rec loop () =
      let n = input stdin buf 0 4096 in
      if n > 0 then (
        Buffer.add_subbytes b buf 0 n;
        loop ()
      )
    in
    loop ();
    Buffer.contents b
  in
  let len = String.length content in
  let tokens = ref [] in
  let i = ref 0 in
  while !i < len do
    while !i < len && (let c = content.[!i] in c = ' ' || c = '\t' || c = '\r' || c = '\n') do
      incr i
    done;
    if !i < len then (
      let start = !i in
      while !i < len && (let c = content.[!i] in c <> ' ' && c <> '\t' && c <> '\r' && c <> '\n') do
        incr i
      done;
      tokens := String.sub content start (!i - start) :: !tokens
    )
  done;
  Array.of_list (List.rev !tokens)

type prod = { text : string; len : int }

let prods = [|
  { text = ""; len = 0 };
  { text = "E -> E + T"; len = 3 };
  { text = "E -> T"; len = 1 };
  { text = "T -> T * F"; len = 3 };
  { text = "T -> F"; len = 1 };
  { text = "F -> ( E )"; len = 3 };
  { text = "F -> id"; len = 1 }
|]

let action_table = Hashtbl.create 32
let goto_table = Hashtbl.create 32

let init_tables () =
  let at = Hashtbl.replace action_table in
  let gt = Hashtbl.replace goto_table in

  at (0, "id") 5;
  at (0, "(") 4;
  gt (0, "E") 1;
  gt (0, "T") 2;
  gt (0, "F") 3;

  at (1, "+") 6;
  at (1, "$") 1000;

  at (2, "+") (-2);
  at (2, "*") 7;
  at (2, ")") (-2);
  at (2, "$") (-2);

  at (3, "+") (-4);
  at (3, "*") (-4);
  at (3, ")") (-4);
  at (3, "$") (-4);

  at (4, "id") 5;
  at (4, "(") 4;
  gt (4, "E") 8;
  gt (4, "T") 2;
  gt (4, "F") 3;

  at (5, "+") (-6);
  at (5, "*") (-6);
  at (5, ")") (-6);
  at (5, "$") (-6);

  at (6, "id") 5;
  at (6, "(") 4;
  gt (6, "T") 9;
  gt (6, "F") 3;

  at (7, "id") 5;
  at (7, "(") 4;
  gt (7, "F") 10;

  at (8, "+") 6;
  at (8, ")") 11;

  at (9, "+") (-1);
  at (9, "*") 7;
  at (9, ")") (-1);
  at (9, "$") (-1);

  at (10, "+") (-3);
  at (10, "*") (-3);
  at (10, ")") (-3);
  at (10, "$") (-3);

  at (11, "+") (-5);
  at (11, "*") (-5);
  at (11, ")") (-5);
  at (11, "$") (-5)

let () =
  init_tables ();
  let tokens = read_all_tokens () in
  if Array.length tokens > 0 then (
    let token_list =
      let lst = Array.to_list tokens in
      if List.nth lst (List.length lst - 1) = "$" then lst
      else lst @ ["$"]
    in
    let token_arr = Array.of_list token_list in
    let state_stack = Stack.create () in
    Stack.push 0 state_stack;
    let ip = ref 0 in
    let stop = ref false in

    while not !stop do
      let s = Stack.top state_stack in
      let a = token_arr.(!ip) in
      match Hashtbl.find_opt action_table (s, a) with
      | None ->
        Printf.printf "SYNTAX_ERROR\n";
        stop := true
      | Some act ->
        if act = 1000 then (
          Printf.printf "ACCEPTED\n";
          stop := true
        ) else if act > 0 then (
          Stack.push act state_stack;
          incr ip
        ) else if act < 0 then (
          let prod_idx = -act in
          Printf.printf "%s\n" prods.(prod_idx).text;
          for _ = 1 to prods.(prod_idx).len do
            ignore (Stack.pop state_stack)
          done;
          let t = Stack.top state_stack in
          let lhs = String.sub prods.(prod_idx).text 0 1 in
          match Hashtbl.find_opt goto_table (t, lhs) with
          | None ->
            Printf.printf "SYNTAX_ERROR\n";
            stop := true
          | Some next_st ->
            Stack.push next_st state_stack
        ) else (
          Printf.printf "SYNTAX_ERROR\n";
          stop := true
        )
    done
  )
