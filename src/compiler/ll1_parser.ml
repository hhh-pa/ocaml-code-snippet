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

let is_terminal c =
  (c < 'A' || c > 'Z') && c <> '#'

module CharSet = Set.Make(Char)

type production = { lhs : char; rhs : string }

let () =
  let tokens = read_all_tokens () in
  if Array.length tokens >= 2 then (
    let num_prods = int_of_string tokens.(0) in
    let start_symbol = tokens.(1).[0] in
    let idx = ref 2 in
    let prods = ref [] in
    let non_terminals = ref CharSet.empty in
    let terminals = ref (CharSet.singleton '$') in

    for _ = 1 to num_prods do
      if !idx + 2 < Array.length tokens then (
        let lhs = tokens.(!idx).[0] in
        let _arrow = tokens.(!idx + 1) in
        let rhs = tokens.(!idx + 2) in
        idx := !idx + 3;
        prods := { lhs; rhs } :: !prods;
        non_terminals := CharSet.add lhs !non_terminals;
        for j = 0 to String.length rhs - 1 do
          let c = rhs.[j] in
          if is_terminal c then
            terminals := CharSet.add c !terminals
          else if c <> '#' then
            non_terminals := CharSet.add c !non_terminals
        done
      )
    done;
    let prods = Array.of_list (List.rev !prods) in

    let first = Hashtbl.create 32 in
    CharSet.iter (fun t -> Hashtbl.replace first t (CharSet.singleton t)) !terminals;

    let get_first c =
      match Hashtbl.find_opt first c with
      | Some s -> s
      | None -> CharSet.empty
    in

    let changed = ref true in
    while !changed do
      changed := false;
      Array.iter (fun prod ->
        let a = prod.lhs in
        let rhs = prod.rhs in
        let old_s = get_first a in
        let cur_s = ref old_s in

        if rhs = "#" then
          cur_s := CharSet.add '#' !cur_s
        else (
          let all_eps = ref true in
          let k = ref 0 in
          let rhs_len = String.length rhs in
          while !k < rhs_len && !all_eps do
            let sym = rhs.[!k] in
            let sym_f = get_first sym in
            let sym_has_eps = CharSet.mem '#' sym_f in
            CharSet.iter (fun f ->
              if f <> '#' then cur_s := CharSet.add f !cur_s
            ) sym_f;
            if not sym_has_eps then all_eps := false;
            incr k
          done;
          if !all_eps then cur_s := CharSet.add '#' !cur_s
        );
        if CharSet.cardinal !cur_s > CharSet.cardinal old_s then (
          Hashtbl.replace first a !cur_s;
          changed := true
        )
      ) prods
    done;

    let first_of_string s =
      if s = "#" || s = "" then
        CharSet.singleton '#'
      else (
        let res = ref CharSet.empty in
        let all_eps = ref true in
        let k = ref 0 in
        let s_len = String.length s in
        while !k < s_len && !all_eps do
          let sym = s.[!k] in
          let sym_f = get_first sym in
          let sym_has_eps = CharSet.mem '#' sym_f in
          CharSet.iter (fun f ->
            if f <> '#' then res := CharSet.add f !res
          ) sym_f;
          if not sym_has_eps then all_eps := false;
          incr k
        done;
        if !all_eps then res := CharSet.add '#' !res;
        !res
      )
    in

    let follow = Hashtbl.create 32 in
    Hashtbl.replace follow start_symbol (CharSet.singleton '$');

    let get_follow c =
      match Hashtbl.find_opt follow c with
      | Some s -> s
      | None -> CharSet.empty
    in

    let changed_f = ref true in
    while !changed_f do
      changed_f := false;
      Array.iter (fun prod ->
        let a = prod.lhs in
        let rhs = prod.rhs in
        if rhs <> "#" then (
          let rhs_len = String.length rhs in
          for i = 0 to rhs_len - 1 do
            let b = rhs.[i] in
            if not (is_terminal b) then (
              let old_s = get_follow b in
              let cur_s = ref old_s in
              let rest = String.sub rhs (i + 1) (rhs_len - 1 - i) in
              let first_rest = first_of_string rest in
              CharSet.iter (fun f ->
                if f <> '#' then cur_s := CharSet.add f !cur_s
              ) first_rest;
              if CharSet.mem '#' first_rest then (
                let fol_a = get_follow a in
                CharSet.iter (fun fol -> cur_s := CharSet.add fol !cur_s) fol_a
              );
              if CharSet.cardinal !cur_s > CharSet.cardinal old_s then (
                Hashtbl.replace follow b !cur_s;
                changed_f := true
              )
            )
          done
        )
      ) prods
    done;

    let table = Hashtbl.create 32 in
    let is_ll1 = ref true in

    Array.iteri (fun p_idx prod ->
      let a = prod.lhs in
      let f_rhs = first_of_string prod.rhs in
      CharSet.iter (fun ch ->
        if ch <> '#' then (
          let key = (a, ch) in
          match Hashtbl.find_opt table key with
          | Some prev_idx when prev_idx <> p_idx -> is_ll1 := false
          | _ -> Hashtbl.replace table key p_idx
        )
      ) f_rhs;
      if CharSet.mem '#' f_rhs then (
        let fol_a = get_follow a in
        CharSet.iter (fun b ->
          let key = (a, b) in
          match Hashtbl.find_opt table key with
          | Some prev_idx when prev_idx <> p_idx -> is_ll1 := false
          | _ -> Hashtbl.replace table key p_idx
        ) fol_a
      )
    ) prods;

    if not !is_ll1 then (
      Printf.printf "GRAMMAR IS NOT LL1\n"
    ) else (
      Printf.printf "GRAMMAR IS LL1\n";
      if !idx < Array.length tokens then (
        let num_queries = int_of_string tokens.(!idx) in
        incr idx;
        for _ = 1 to num_queries do
          if !idx < Array.length tokens then (
            let w = tokens.(!idx) ^ "$" in
            incr idx;
            let stack = ref ['$'; start_symbol] in
            let ip = ref 0 in
            let derivations = ref [] in
            let ok = ref true in
            let w_len = String.length w in

            while !ok && !stack <> [] do
              let x = List.hd !stack in
              stack := List.tl !stack;
              let a = if !ip < w_len then w.[!ip] else '$' in
              if x = a then (
                incr ip;
                if x = '$' then stack := []
              ) else if is_terminal x then (
                ok := false
              ) else (
                let key = (x, a) in
                match Hashtbl.find_opt table key with
                | None -> ok := false
                | Some rule_idx ->
                  derivations := rule_idx :: !derivations;
                  let rhs = prods.(rule_idx).rhs in
                  if rhs <> "#" then (
                    let rhs_len = String.length rhs in
                    for k = rhs_len - 1 downto 0 do
                      stack := rhs.[k] :: !stack
                    done
                  )
              )
            done;
            if !ok && !ip = w_len then (
              let d_list = List.rev !derivations in
              let d_strs = List.map (fun d -> "r" ^ string_of_int d) d_list in
              Printf.printf "%s\n" (String.concat " " d_strs)
            ) else (
              Printf.printf "SYNTAX ERROR\n"
            )
          )
        done
      )
    )
  )
