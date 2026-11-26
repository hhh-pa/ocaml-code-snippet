(* Regev LWE public-key encryption (toy parameters). *)

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
  | op :: rest -> (
      if op = "ENC" then
        match rest with
        | ns :: qs :: mus :: tl ->
            let n = int_of_string ns
            and q = Int64.of_string qs
            and mu = Int64.of_string mus in
            let vals = Array.of_list (List.map Int64.of_string tl) in
            let m = Array.length vals / (n + 2) in
            let parts = ref [] in
            for j = 0 to n - 1 do
              let uj = ref 0L in
              for i = 0 to m - 1 do
                uj :=
                  Int64.rem
                    (Int64.add !uj
                       (Int64.mul vals.((m * n) + m + i) vals.((i * n) + j)))
                    q
              done;
              parts :=
                Printf.sprintf "%Ld"
                  (Int64.rem (Int64.add (Int64.rem !uj q) q) q)
                :: !parts
            done;
            let vsum = ref 0L in
            for i = 0 to m - 1 do
              vsum :=
                Int64.rem
                  (Int64.add !vsum
                     (Int64.mul vals.((m * n) + m + i) vals.((m * n) + i)))
                  q
            done;
            parts :=
              Printf.sprintf "%Ld"
                (Int64.rem (Int64.add !vsum (Int64.mul mu (Int64.div q 2L))) q)
              :: !parts;
            print_string (String.concat " " (List.rev !parts))
        | _ -> ()
      else
        match rest with
        | ns :: qs :: tl ->
            let n = int_of_string ns and q = Int64.of_string qs in
            let vals = Array.of_list (List.map Int64.of_string tl) in
            let x = ref vals.(2 * n) in
            for j = 0 to n - 1 do
              x := Int64.rem (Int64.sub !x (Int64.mul vals.(j) vals.(n + j))) q
            done;
            x := Int64.rem (Int64.add (Int64.rem !x q) q) q;
            let mn = min !x (Int64.sub q !x) in
            print_string (if Int64.mul mn 4L < q then "0" else "1")
        | _ -> ())
  | _ -> ()
