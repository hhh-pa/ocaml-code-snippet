let digits = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"

let () =
  try
    let sb = Scanf.scanf " %d" (fun x -> x) in
    let db = Scanf.scanf " %d" (fun x -> x) in
    let dg = Scanf.scanf " %s" (fun x -> x) in
    let cur =
      ref
        (Array.init (String.length dg) (fun i ->
             String.index digits (Char.uppercase_ascii dg.[i])))
    in
    if Array.length !cur = 0 || Array.for_all (fun d -> d = 0) !cur then
      print_string "0"
    else begin
      let out = Buffer.create 16 in
      while Array.length !cur > 0 do
        let nxt = ref [] in
        let carry = ref 0 in
        Array.iter
          (fun d ->
            let v = (!carry * sb) + d in
            let q = v / db in
            carry := v mod db;
            if !nxt <> [] || q <> 0 then nxt := q :: !nxt)
          !cur;
        Buffer.add_char out digits.[!carry];
        cur := Array.of_list (List.rev !nxt)
      done;
      let s = Buffer.contents out in
      for i = String.length s - 1 downto 0 do
        print_char s.[i]
      done
    end
  with End_of_file -> ()
