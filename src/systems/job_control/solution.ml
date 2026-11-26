(* Shell job control (CS:APP ch 8 + Shell Lab). *)

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

type job = { pid : int; mutable state : string; cmd : string }

let find_fg jobs =
  List.find_opt (fun (_, j) -> j.state = "FG") jobs |> Option.map fst

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
      let e = int_of_string (next ()) in
      let jobs = ref [] in
      let nextjid = ref 1 in
      let out = Buffer.create 256 in
      let emit s = Buffer.add_string out (s ^ "\n") in
      for _ = 1 to e do
        let op = next () in
        if op = "RUN" then
          let pid = int_of_string (next ()) in
          let cmd = next () in
          let bg = int_of_string (next ()) in
          if bg = 0 && find_fg !jobs <> None then emit "REJECTED"
          else (
            jobs :=
              !jobs
              @ [
                  ( !nextjid,
                    { pid; state = (if bg = 0 then "FG" else "BG"); cmd } );
                ];
            emit (Printf.sprintf "JOB %d %d" !nextjid pid);
            incr nextjid)
        else if op = "INT" || op = "TSTP" then
          match find_fg !jobs with
          | None -> emit "NOJOB"
          | Some f ->
              if op = "INT" then (
                jobs := List.filter (fun (jid, _) -> jid <> f) !jobs;
                emit (Printf.sprintf "TERM %d" f))
              else (
                (List.assoc f !jobs).state <- "ST";
                emit (Printf.sprintf "STOP %d" f))
        else if op = "FG" then (
          let jid = int_of_string (next ()) in
          if not (List.mem_assoc jid !jobs) then emit "NOJOB"
          else
            match find_fg !jobs with
            | Some f when f <> jid -> emit "REJECTED"
            | _ ->
                (List.assoc jid !jobs).state <- "FG";
                emit (Printf.sprintf "FG %d" jid))
        else if op = "BG" then
          let jid = int_of_string (next ()) in
          if not (List.mem_assoc jid !jobs) then emit "NOJOB"
          else if (List.assoc jid !jobs).state = "FG" then emit "REJECTED"
          else (
            (List.assoc jid !jobs).state <- "BG";
            emit (Printf.sprintf "BG %d" jid))
        else if op = "KILL" then
          let jid = int_of_string (next ()) in
          if not (List.mem_assoc jid !jobs) then emit "NOJOB"
          else (
            jobs := List.filter (fun (jid2, _) -> jid2 <> jid) !jobs;
            emit (Printf.sprintf "TERM %d" jid))
        else if op = "EXIT" then (
          let pid = int_of_string (next ()) in
          match List.find_opt (fun (_, j) -> j.pid = pid) !jobs with
          | None -> emit "NOJOB"
          | Some (jid, _) ->
              jobs := List.filter (fun (jid2, _) -> jid2 <> jid) !jobs;
              emit (Printf.sprintf "DONE %d" jid))
        else
          match !jobs with
          | [] -> emit "NOJOBS"
          | _ ->
              List.iter
                (fun (jid, j) ->
                  emit (Printf.sprintf "%d %d %s %s" jid j.pid j.state j.cmd))
                !jobs
      done;
      print_string (Buffer.contents out)
