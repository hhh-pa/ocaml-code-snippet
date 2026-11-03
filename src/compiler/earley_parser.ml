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

let is_terminal_symbol c =
  (c < 'A' || c > 'Z') && c <> '#'

type production = {
  lhs : char;
  rhs : string;
}

type item = {
  rule_id : int;
  dot : int;
  origin : int;
}

let () =
  let tokens = read_all_tokens () in
  if Array.length tokens >= 2 then (
    let num_rules = int_of_string tokens.(0) in
    let start_symbol = tokens.(1).[0] in
    let idx = ref 2 in
    let prods = ref [{ lhs = '@'; rhs = String.make 1 start_symbol }] in
    for _ = 1 to num_rules do
      if !idx + 2 < Array.length tokens then (
        let lhs = tokens.(!idx).[0] in
        let _arrow = tokens.(!idx + 1) in
        let rhs_str = tokens.(!idx + 2) in
        idx := !idx + 3;
        let rhs = if rhs_str = "#" then "" else rhs_str in
        prods := { lhs; rhs } :: !prods
      )
    done;
    let prods = Array.of_list (List.rev !prods) in
    let num_prods = Array.length prods in

    if !idx < Array.length tokens then (
      let num_queries = int_of_string tokens.(!idx) in
      incr idx;
      for _ = 1 to num_queries do
        if !idx < Array.length tokens then (
          let w_str = tokens.(!idx) in
          incr idx;
          let w = if w_str = "EPS" then "" else w_str in
          let n = String.length w in

          let chart = Array.init (n + 1) (fun _ -> ref []) in
          let in_chart = Array.init (n + 1) (fun _ -> Hashtbl.create 32) in

          let add_item k it =
            let key = (it.rule_id, it.dot, it.origin) in
            if not (Hashtbl.mem in_chart.(k) key) then (
              Hashtbl.replace in_chart.(k) key ();
              chart.(k) := it :: !(chart.(k))
            )
          in

          add_item 0 { rule_id = 0; dot = 0; origin = 0 };

          let total_items = ref 0 in

          for k = 0 to n do
            (* chart.(k) grows dynamically, process via queue or indexed loop *)
            let q = Queue.create () in
            List.iter (fun it -> Queue.push it q) (List.rev !(chart.(k)));
            let visited_in_k = Hashtbl.create 32 in

            let add_item_q target_k it =
              let key = (it.rule_id, it.dot, it.origin) in
              if not (Hashtbl.mem in_chart.(target_k) key) then (
                Hashtbl.replace in_chart.(target_k) key ();
                chart.(target_k) := it :: !(chart.(target_k));
                if target_k = k then Queue.push it q
              )
            in

            while not (Queue.is_empty q) do
              let it = Queue.pop q in
              let key = (it.rule_id, it.dot, it.origin) in
              if not (Hashtbl.mem visited_in_k key) then (
                Hashtbl.replace visited_in_k key ();
                let prod = prods.(it.rule_id) in
                let rhs_len = String.length prod.rhs in
                if it.dot < rhs_len then (
                  let next_sym = prod.rhs.[it.dot] in
                  if not (is_terminal_symbol next_sym) then (
                    (* Predictor *)
                    for r = 0 to num_prods - 1 do
                      if prods.(r).lhs = next_sym then
                        add_item_q k { rule_id = r; dot = 0; origin = k }
                    done
                  ) else (
                    (* Scanner *)
                    if k < n && w.[k] = next_sym then
                      add_item_q (k + 1) { rule_id = it.rule_id; dot = it.dot + 1; origin = it.origin }
                  )
                ) else (
                  (* Completer *)
                  let completed_lhs = prod.lhs in
                  let origin_k = it.origin in
                  let origin_items = List.rev !(chart.(origin_k)) in
                  List.iter (fun prev ->
                    let prev_prod = prods.(prev.rule_id) in
                    let prev_rhs_len = String.length prev_prod.rhs in
                    if prev.dot < prev_rhs_len && prev_prod.rhs.[prev.dot] = completed_lhs then
                      add_item_q k { rule_id = prev.rule_id; dot = prev.dot + 1; origin = prev.origin }
                  ) origin_items
                )
              )
            done;
            total_items := !total_items + List.length !(chart.(k))
          done;

          let accepted = Hashtbl.mem in_chart.(n) (0, 1, 0) in
          if accepted then
            Printf.printf "ACCEPT %d\n" !total_items
          else
            Printf.printf "REJECT %d\n" !total_items
        )
      done
    )
  )
