(* Smith-Waterman local sequence alignment (general scoring). *)

let read_all () =
  let buf = Buffer.create 256 in
  (try
     while true do
       Buffer.add_char buf (input_char stdin)
     done
   with End_of_file -> ());
  Buffer.contents buf

let split_fields s =
  let parts = ref [] in
  let cur = Buffer.create 16 in
  String.iter
    (fun c ->
      if c = ' ' then begin
        parts := Buffer.contents cur :: !parts;
        Buffer.clear cur
      end
      else Buffer.add_char cur c)
    s;
  parts := Buffer.contents cur :: !parts;
  List.rev !parts

let () =
  match split_fields (String.trim (read_all ())) with
  | _ :: ms :: mms :: gs :: s1 :: s2 :: _ ->
      let vmatch = int_of_string ms
      and mismatch = int_of_string mms
      and gap = int_of_string gs in
      let n = String.length s1 and m = String.length s2 in
      let f = Array.make_matrix (n + 1) (m + 1) 0 in
      let best = ref 0 and bi = ref 0 and bj = ref 0 in
      for i = 1 to n do
        for j = 1 to m do
          let sub = if s1.[i - 1] = s2.[j - 1] then vmatch else mismatch in
          f.(i).(j) <-
            max 0
              (max
                 (f.(i - 1).(j - 1) + sub)
                 (max (f.(i - 1).(j) + gap) (f.(i).(j - 1) + gap)));
          if f.(i).(j) > !best then begin
            best := f.(i).(j);
            bi := i;
            bj := j
          end
        done
      done;
      let a1 = Buffer.create 16 and a2 = Buffer.create 16 in
      let i = ref !bi and j = ref !bj in
      while !i > 0 && !j > 0 && f.(!i).(!j) > 0 do
        let sub = if s1.[!i - 1] = s2.[!j - 1] then vmatch else mismatch in
        if f.(!i).(!j) = f.(!i - 1).(!j - 1) + sub then begin
          Buffer.add_char a1 s1.[!i - 1];
          Buffer.add_char a2 s2.[!j - 1];
          decr i;
          decr j
        end
        else if f.(!i).(!j) = f.(!i - 1).(!j) + gap then begin
          Buffer.add_char a1 s1.[!i - 1];
          Buffer.add_char a2 '-';
          decr i
        end
        else if f.(!i).(!j) = f.(!i).(!j - 1) + gap then begin
          Buffer.add_char a1 '-';
          Buffer.add_char a2 s2.[!j - 1];
          decr j
        end
        else begin
          i := 0;
          j := 0
        end
      done;
      let rev b =
        let s = Buffer.contents b in
        String.init (String.length s) (fun k -> s.[String.length s - 1 - k])
      in
      let r1 = rev a1 and r2 = rev a2 in
      if r1 = "" && r2 = "" then Printf.printf "%d" !best
      else Printf.printf "%d\n%s\n%s" !best r1 r2
  | _ -> ()
