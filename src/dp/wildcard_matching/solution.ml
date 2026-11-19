(* Wildcard matching with ? and * (O(n*m) DP, full match). *)

let wc_match text pat =
  let n = String.length text and m = String.length pat in
  let dp = Array.make (m + 1) false in
  dp.(0) <- true;
  for j = 1 to m do
    dp.(j) <- dp.(j - 1) && pat.[j - 1] = '*'
  done;
  let ndp = Array.make (m + 1) false in
  for i = 1 to n do
    ndp.(0) <- false;
    for j = 1 to m do
      let c = pat.[j - 1] in
      ndp.(j) <-
        (if c = '*' then ndp.(j - 1) || dp.(j)
         else if c = '?' || c = text.[i - 1] then dp.(j - 1)
         else false)
    done;
    Array.blit ndp 0 dp 0 (m + 1)
  done;
  dp.(m)

let read_all () =
  let buf = Buffer.create 256 in
  (try
     while true do
       Buffer.add_char buf (input_char stdin)
     done
   with End_of_file -> ());
  Buffer.contents buf

let () =
  let parts = String.split_on_char '\n' (read_all ()) in
  match parts with
  | [] -> ()
  | qs :: _ when String.trim qs = "" -> ()
  | qs :: rest ->
      let q = int_of_string (String.trim qs) in
      let lines =
        Array.init (2 * q) (fun i ->
            try List.nth rest i with Failure _ | Invalid_argument _ -> "")
      in
      let strip s =
        let n = String.length s in
        if n > 0 && s.[n - 1] = '\r' then String.sub s 0 (n - 1) else s
      in
      let out = ref [] in
      for i = 0 to q - 1 do
        let text = strip lines.(2 * i) and pat = strip lines.((2 * i) + 1) in
        out := (if wc_match text pat then "YES" else "NO") :: !out
      done;
      print_string (String.concat "\n" (List.rev !out))
