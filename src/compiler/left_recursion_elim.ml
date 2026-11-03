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

module StringMap = Map.Make(String)

type alt = string list
type rule = { mutable lhs : string; mutable alts : alt list }

let syms_of (s : string) : string list =
  let r = ref [] in
  let i = ref 0 in
  let n = String.length s in
  while !i < n do
    if s.[!i] >= 'A' && s.[!i] <= 'Z' then (
      let t = Buffer.create 4 in
      Buffer.add_char t s.[!i];
      incr i;
      while !i < n && s.[!i] = '\'' do
        Buffer.add_char t s.[!i];
        incr i
      done;
      r := Buffer.contents t :: !r
    ) else (
      r := String.make 1 s.[!i] :: !r;
      incr i
    )
  done;
  List.rev !r

let show_alt (a : alt) : string =
  if a = [] then "@" else String.concat "" a

let fresh_for (base : string) (rules : rule list) : string =
  let b = ref base in
  let found = ref false in
  while not !found do
    b := !b ^ "'";
    let used = List.exists (fun (rl : rule) -> rl.lhs = !b) rules in
    if not used then found := true
  done;
  !b

let split_by_bar (s : string) : string list =
  let res = ref [] in
  let start = ref 0 in
  let len = String.length s in
  for i = 0 to len - 1 do
    if s.[i] = '|' then (
      res := String.sub s !start (i - !start) :: !res;
      start := i + 1
    )
  done;
  res := String.sub s !start (len - !start) :: !res;
  List.rev !res

let () =
  let tokens = read_all_tokens () in
  if Array.length tokens >= 1 then (
    let t_cases = int_of_string tokens.(0) in
    let idx = ref 1 in
    for _ = 1 to t_cases do
      if !idx < Array.length tokens then (
        let r_count = int_of_string tokens.(!idx) in
        incr idx;
        let rules = ref [] in
        for _ = 1 to r_count do
          if !idx < Array.length tokens then (
            let line = tokens.(!idx) in
            incr idx;
            let eq = String.index line '=' in
            let lhs = String.sub line 0 eq in
            let rest = String.sub line (eq + 1) (String.length line - eq - 1) in
            let parts = split_by_bar rest in
            let alts = List.map (fun a ->
              if a = "@" || a = "" then [] else syms_of a
            ) parts in
            rules := { lhs; alts } :: !rules
          )
        done;
        let rules_list = ref (List.rev !rules) in

        let n0 = List.length !rules_list in
        let rules_arr = Array.of_list !rules_list in

        for i = 0 to n0 - 1 do
          for j = 0 to i - 1 do
            let na = ref [] in
            List.iter (fun (a : alt) ->
              match a with
              | hd :: tl when hd = rules_arr.(j).lhs ->
                List.iter (fun (b : alt) ->
                  na := (b @ tl) :: !na
                ) rules_arr.(j).alts
              | _ -> na := a :: !na
            ) rules_arr.(i).alts;
            rules_arr.(i).alts <- List.rev !na
          done;

          (* dedup *)
          let dd = ref [] in
          List.iter (fun a ->
            if not (List.mem a !dd) then dd := a :: !dd
          ) rules_arr.(i).alts;
          rules_arr.(i).alts <- List.rev !dd;

          (* immediate elimination *)
          let rec_alts = ref [] in
          let nonrec_alts = ref [] in
          List.iter (fun (a : alt) ->
            match a with
            | hd :: tl when hd = rules_arr.(i).lhs ->
              rec_alts := tl :: !rec_alts
            | _ -> nonrec_alts := a :: !nonrec_alts
          ) rules_arr.(i).alts;
          let rec_alts = List.rev !rec_alts in
          let nonrec_alts = List.rev !nonrec_alts in

          if rec_alts <> [] then (
            let cur_all_rules = Array.to_list rules_arr in
            let np = fresh_for rules_arr.(i).lhs (!rules_list @ cur_all_rules) in
            let nalts =
              if nonrec_alts = [] then [[np]]
              else List.map (fun (b : alt) -> b @ [np]) nonrec_alts
            in
            rules_arr.(i).alts <- nalts;
            let palts = ref (List.map (fun (d : alt) -> d @ [np]) rec_alts) in
            if nonrec_alts <> [] then palts := !palts @ [[]];
            let new_rule = { lhs = np; alts = !palts } in
            rules_list := !rules_list @ [new_rule]
          )
        done;

        (* left factoring to fixed point *)
        let all_rules = ref (Array.to_list rules_arr @ (
          let rec drop k lst = if k = 0 then lst else match lst with [] -> [] | _ :: t -> drop (k - 1) t in
          drop n0 !rules_list
        )) in

        for _round = 1 to 20 do
          let any = ref false in
          let n_cur = List.length !all_rules in
          let rules_vec = Array.of_list !all_rules in
          let i = ref 0 in
          while !i < n_cur && not !any do
            let cur_rule = rules_vec.(!i) in
            let groups = ref StringMap.empty in
            List.iteri (fun k (a : alt) ->
              let key = match a with [] -> "" | hd :: _ -> hd in
              let prev = match StringMap.find_opt key !groups with Some lst -> lst | None -> [] in
              groups := StringMap.add key (k :: prev) !groups
            ) cur_rule.alts;

            let group_bindings = StringMap.bindings !groups in
            let g_it = ref group_bindings in
            while !g_it <> [] && not !any do
              let (key, idx_list_rev) = List.hd !g_it in
              g_it := List.tl !g_it;
              let idx_list = List.rev idx_list_rev in
              if List.length idx_list >= 2 then (
                let g = Array.of_list (List.map (fun k -> List.nth cur_rule.alts k) idx_list) in
                let pre = ref 0 in
                let ok = ref true in
                let g0_len = List.length g.(0) in
                while !ok && !pre < g0_len do
                  for k = 1 to Array.length g - 1 do
                    let gk_len = List.length g.(k) in
                    if !pre >= gk_len || List.nth g.(k) !pre <> List.nth g.(0) !pre then
                      ok := false
                  done;
                  if !ok then incr pre
                done;
                if !pre > 0 then (
                  let np = fresh_for cur_rule.lhs !all_rules in
                  let rec take k lst = if k = 0 then [] else match lst with [] -> [] | h :: t -> h :: take (k - 1) t in
                  let rec drop k lst = if k = 0 then lst else match lst with [] -> [] | _ :: t -> drop (k - 1) t in
                  let prefix = take !pre g.(0) in
                  let first_new = prefix @ [np] in
                  let keep = ref [] in
                  let suflist = ref [] in
                  let in_g = Array.make (List.length cur_rule.alts) false in
                  List.iter (fun k -> in_g.(k) <- true) idx_list;
                  let placed = ref false in
                  List.iteri (fun k alt ->
                    if in_g.(k) then (
                      if not !placed then (
                        keep := first_new :: !keep;
                        placed := true
                      );
                      let suf = drop !pre alt in
                      suflist := suf :: !suflist
                    ) else
                      keep := alt :: !keep
                  ) cur_rule.alts;
                  cur_rule.alts <- List.rev !keep;
                  let new_rule = { lhs = np; alts = List.rev !suflist } in
                  all_rules := Array.to_list rules_vec @ [new_rule];
                  any := true
                )
              )
            done;
            incr i
          done
        done;

        Printf.printf "GRAMMAR %d\n" (List.length !all_rules);
        List.iter (fun rl ->
          Printf.printf "%s=" rl.lhs;
          let alt_strs = List.map show_alt rl.alts in
          Printf.printf "%s\n" (String.concat "|" alt_strs)
        ) !all_rules
      )
    done
  )
