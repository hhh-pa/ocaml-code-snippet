(* Needleman-Wunsch global sequence alignment (general scoring). *)

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
  | _ :: ms :: mms :: gs :: rest ->
      let vmatch = int_of_string ms
      and mismatch = int_of_string mms
      and gap = int_of_string gs in
      let s1 = match rest with a :: _ -> a | [] -> "" in
      let s2 = match rest with _ :: b :: _ -> b | _ -> "" in
      let n = String.length s1 and m = String.length s2 in
      let f = Array.make_matrix (n + 1) (m + 1) 0 in
      for i = 1 to n do
        f.(i).(0) <- i * gap
      done;
      for j = 1 to m do
        f.(0).(j) <- j * gap
      done;
      for i = 1 to n do
        for j = 1 to m do
          let sub = if s1.[i - 1] = s2.[j - 1] then vmatch else mismatch in
          f.(i).(j) <-
            max
              (f.(i - 1).(j - 1) + sub)
              (max (f.(i - 1).(j) + gap) (f.(i).(j - 1) + gap))
        done
      done;
      let a1 = Buffer.create 16 and a2 = Buffer.create 16 in
      let i = ref n and j = ref m in
      while !i > 0 || !j > 0 do
        if !i > 0 && !j > 0 then begin
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
          else begin
            Buffer.add_char a1 '-';
            Buffer.add_char a2 s2.[!j - 1];
            decr j
          end
        end
        else if !i > 0 then begin
          Buffer.add_char a1 s1.[!i - 1];
          Buffer.add_char a2 '-';
          decr i
        end
        else begin
          Buffer.add_char a1 '-';
          Buffer.add_char a2 s2.[!j - 1];
          decr j
        end
      done;
      let rev b =
        let s = Buffer.contents b in
        String.init (String.length s) (fun k -> s.[String.length s - 1 - k])
      in
      Printf.printf "%d\n%s\n%s" f.(n).(m) (rev a1) (rev a2)
  | _ -> ()
