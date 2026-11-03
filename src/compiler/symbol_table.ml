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
    let n = int_of_string (String.trim lines.(!idx)) in
    incr idx;
    let scopes = ref [Hashtbl.create 8] in
    let chain = ref [0] in
    let nextid = ref 1 in
    let declares = ref 0 in
    let errors = ref 0 in
    let count = ref 0 in

    while !idx < num_lines && !count < n do
      let line = lines.(!idx) in
      incr idx;
      let trimmed = String.trim line in
      if trimmed <> "" then (
        incr count;
        let tokens = Array.of_list (
          let raw = String.split_on_char ' ' trimmed in
          List.filter (fun s -> s <> "") raw
        ) in
        let cmd = tokens.(0) in
        if cmd = "ENTER" then (
          scopes := Hashtbl.create 8 :: !scopes;
          chain := !chain @ [!nextid];
          incr nextid;
          Printf.printf "ENTER %d\n" (List.length !scopes - 1)
        ) else if cmd = "EXIT" then (
          if List.length !scopes <= 1 then (
            Printf.printf "EXIT UNDERFLOW\n";
            incr errors
          ) else (
            scopes := List.tl !scopes;
            let c_arr = Array.of_list !chain in
            chain := Array.to_list (Array.sub c_arr 0 (Array.length c_arr - 1));
            Printf.printf "EXIT %d\n" (List.length !scopes - 1)
          )
        ) else if cmd = "DECLARE" && Array.length tokens >= 3 then (
          let name = tokens.(1) in
          let typ = tokens.(2) in
          let cur_scope = List.hd !scopes in
          if Hashtbl.mem cur_scope name then (
            Printf.printf "DECLARE %s %s REDECLARED\n" name typ;
            incr errors
          ) else (
            Hashtbl.replace cur_scope name typ;
            Printf.printf "DECLARE %s %s OK\n" name typ;
            incr declares
          )
        ) else if cmd = "LOOKUP" && Array.length tokens >= 2 then (
          let name = tokens.(1) in
          let found = ref false in
          let scopes_arr = Array.of_list (List.rev !scopes) in
          let d = ref (Array.length scopes_arr - 1) in
          while !d >= 0 && not !found do
            match Hashtbl.find_opt scopes_arr.(!d) name with
            | Some typ ->
              Printf.printf "LOOKUP %s %s %d\n" name typ !d;
              found := true
            | None -> decr d
          done;
          if not !found then (
            Printf.printf "LOOKUP %s UNDECLARED\n" name;
            incr errors
          )
        ) else if cmd = "ASSIGN" && Array.length tokens >= 2 then (
          let name = tokens.(1) in
          let found = ref false in
          let scopes_arr = Array.of_list (List.rev !scopes) in
          let d = ref (Array.length scopes_arr - 1) in
          while !d >= 0 && not !found do
            if Hashtbl.mem scopes_arr.(!d) name then (
              Printf.printf "ASSIGN %s OK %d\n" name !d;
              found := true
            ) else
              decr d
          done;
          if not !found then (
            Printf.printf "ASSIGN %s UNDECLARED\n" name;
            incr errors
          )
        ) else if cmd = "DISPLAY" then (
          Printf.printf "DISPLAY";
          List.iteri (fun d cid ->
            Printf.printf " %d:%d" d cid
          ) !chain;
          print_newline ()
        )
      )
    done;
    Printf.printf "SYMTAB declares=%d errors=%d\n" !declares !errors
  )
