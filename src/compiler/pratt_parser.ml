type token_type =
  | NUMBER of int
  | PLUS
  | MINUS
  | STAR
  | SLASH
  | CARET
  | LPAREN
  | RPAREN
  | END_OF_INPUT

type ast_node =
  | Num of int
  | Unary of string * ast_node
  | Binary of string * ast_node * ast_node

let rec to_sexpr = function
  | Num v -> string_of_int v
  | Unary (op, child) -> "(" ^ op ^ " " ^ to_sexpr child ^ ")"
  | Binary (op, left, right) -> "(" ^ op ^ " " ^ to_sexpr left ^ " " ^ to_sexpr right ^ ")"

let rec eval_ast = function
  | Num v -> Int64.of_int v
  | Unary (op, child) ->
    let v = eval_ast child in
    if op = "-" then Int64.neg v else v
  | Binary (op, left, right) ->
    let l = eval_ast left in
    let r = eval_ast right in
    match op with
    | "+" -> Int64.add l r
    | "-" -> Int64.sub l r
    | "*" -> Int64.mul l r
    | "/" ->
      if r = 0L then 0L else Int64.div l r
    | "^" ->
      let res = ref 1L in
      let count = Int64.to_int r in
      for _ = 1 to count do
        res := Int64.mul !res l
      done;
      !res
    | _ -> 0L

let parse_pratt line =
  let pos = ref 0 in
  let len = String.length line in

  let next_token () =
    while !pos < len && (let c = line.[!pos] in c = ' ' || c = '\t' || c = '\r' || c = '\n') do
      incr pos
    done;
    if !pos >= len then END_OF_INPUT
    else
      let c = line.[!pos] in
      if c >= '0' && c <= '9' then (
        let v = ref 0 in
        while !pos < len && line.[!pos] >= '0' && line.[!pos] <= '9' do
          v := !v * 10 + (Char.code line.[!pos] - Char.code '0');
          incr pos
        done;
        NUMBER !v
      ) else (
        incr pos;
        match c with
        | '+' -> PLUS
        | '-' -> MINUS
        | '*' -> STAR
        | '/' -> SLASH
        | '^' -> CARET
        | '(' -> LPAREN
        | ')' -> RPAREN
        | _ -> END_OF_INPUT
      )
  in

  let curr = ref (next_token ()) in
  let advance () = curr := next_token () in

  let infix_binding_power = function
    | PLUS | MINUS -> (10, 11)
    | STAR | SLASH -> (20, 21)
    | CARET -> (31, 30)
    | _ -> (-1, -1)
  in

  let rec parse_expression min_bp =
    let lhs_opt =
      match !curr with
      | NUMBER v ->
        advance ();
        Some (Num v)
      | MINUS ->
        advance ();
        (match parse_expression 40 with
         | None -> None
         | Some rhs -> Some (Unary ("-", rhs)))
      | LPAREN ->
        advance ();
        let inner = parse_expression 0 in
        if !curr = RPAREN then advance ();
        inner
      | _ -> None
    in
    match lhs_opt with
    | None -> None
    | Some lhs ->
      let rec loop cur_lhs =
        let l_bp, r_bp = infix_binding_power !curr in
        if l_bp < min_bp then Some cur_lhs
        else
          let op_str =
            match !curr with
            | PLUS -> "+"
            | MINUS -> "-"
            | STAR -> "*"
            | SLASH -> "/"
            | CARET -> "^"
            | _ -> ""
          in
          advance ();
          match parse_expression r_bp with
          | None -> None
          | Some rhs -> loop (Binary (op_str, cur_lhs, rhs))
      in
      loop lhs
  in
  parse_expression 0

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
    let num_exprs = int_of_string (String.trim lines.(!idx)) in
    incr idx;
    let count = ref 0 in
    while !idx < num_lines && !count < num_exprs do
      let line = lines.(!idx) in
      incr idx;
      if String.trim line <> "" then (
        incr count;
        match parse_pratt line with
        | Some ast ->
          let sexpr = to_sexpr ast in
          let res = eval_ast ast in
          Printf.printf "%s = %Ld\n" sexpr res
        | None ->
          Printf.printf "PARSE_ERROR\n"
      )
    done
  )
