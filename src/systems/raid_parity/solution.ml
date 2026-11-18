(* RAID-5 parity + single-disk reconstruction (MOS 5.4). *)

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
  | [] -> ()
  | toks ->
      let t = Array.of_list toks in
      let pos = ref 0 in
      let next () =
        let v = t.(!pos) in
        incr pos;
        v
      in
      let drives = int_of_string (next ()) in
      let stripes = int_of_string (next ()) in
      let e = int_of_string (next ()) in
      let data = Array.make_matrix stripes drives 0 in
      let par = Array.make stripes 0 in
      let failed = Array.make drives false in
      let nfailed = ref 0 in
      let out = Buffer.create 256 in
      let slot s d = if d = s mod drives then par.(s) else data.(s).(d) in
      for _ = 1 to e do
        let op = next () in
        if op = "W" then
          let s = int_of_string (next ()) in
          let d = int_of_string (next ()) in
          let v = int_of_string (next ()) in
          if
            s < 0 || s >= stripes || d < 0 || d >= drives
            || d = s mod drives
            || failed.(d)
          then Buffer.add_string out "REJECTED\n"
          else (
            par.(s) <- par.(s) lxor data.(s).(d) lxor v;
            data.(s).(d) <- v;
            Buffer.add_string out "OK\n")
        else if op = "R" then (
          let s = int_of_string (next ()) in
          let d = int_of_string (next ()) in
          if s < 0 || s >= stripes || d < 0 || d >= drives then
            Buffer.add_string out "REJECTED\n"
          else if failed.(d) && !nfailed > 1 then
            Buffer.add_string out "UNRECOVERABLE\n"
          else if not failed.(d) then
            Buffer.add_string out (Printf.sprintf "BYTE %d\n" (slot s d))
          else
            let x = ref 0 in
            for dd = 0 to drives - 1 do
              if dd <> d then x := !x lxor slot s dd
            done;
            Buffer.add_string out (Printf.sprintf "BYTE %d\n" !x))
        else if op = "F" then
          let d = int_of_string (next ()) in
          if d < 0 || d >= drives then Buffer.add_string out "REJECTED\n"
          else (
            if not failed.(d) then begin
              failed.(d) <- true;
              incr nfailed
            end;
            Buffer.add_string out "OK\n")
        else
          let s = int_of_string (next ()) in
          if s < 0 || s >= stripes then Buffer.add_string out "REJECTED\n"
          else
            let pd = s mod drives in
            if failed.(pd) && !nfailed > 1 then
              Buffer.add_string out "UNRECOVERABLE\n"
            else if not failed.(pd) then
              Buffer.add_string out (Printf.sprintf "PARITY %d %d\n" pd par.(s))
            else
              let x = ref 0 in
              for dd = 0 to drives - 1 do
                if dd <> pd then x := !x lxor data.(s).(dd)
              done;
              Buffer.add_string out (Printf.sprintf "PARITY %d %d\n" pd !x)
      done;
      print_string (Buffer.contents out)
