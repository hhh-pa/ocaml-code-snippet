(* Myers O(ND) shortest edit script (insert/delete only). *)

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

module IM = Map.Make (Int)

let vget v k = try IM.find k v with Not_found -> -1

let () =
  match split_fields (String.trim (read_all ())) with
  | _ :: rest ->
      let a = match rest with x :: _ -> x | [] -> "" in
      let b = match rest with _ :: y :: _ -> y | _ -> "" in
      let n = String.length a and m = String.length b in
      let trace = ref []
      and v = ref (IM.add 1 0 IM.empty)
      and found = ref (-1) in
      let d = ref 0 in
      while !found < 0 do
        let cur = ref IM.empty in
        let k = ref (- !d) in
        while !k <= !d && !found < 0 do
          let kk = !k in
          let x =
            if kk = - !d || (kk <> !d && vget !v (kk - 1) < vget !v (kk + 1))
            then try IM.find (kk + 1) !v with Not_found -> 0
            else (try IM.find (kk - 1) !v with Not_found -> 0) + 1
          in
          let x = ref x and y = ref (x - kk) in
          while !x < n && !y < m && a.[!x] = b.[!y] do
            incr x;
            incr y
          done;
          cur := IM.add kk !x !cur;
          if !x >= n && !y >= m then found := !d;
          k := !k + 2
        done;
        trace := !v :: !trace;
        v := !cur;
        incr d
      done;
      let trace = Array.of_list (List.rev !trace) in
      let ops = Buffer.create 16 in
      let x = ref n and y = ref m in
      for dd = !found downto 1 do
        let vv = trace.(dd) and k = !x - !y in
        if k = -dd || (k <> dd && vget vv (k - 1) < vget vv (k + 1)) then begin
          let px = IM.find (k + 1) vv in
          while !x > px do
            Buffer.add_char ops '=';
            decr x;
            decr y
          done;
          Buffer.add_char ops '+';
          decr y
        end
        else begin
          let px = IM.find (k - 1) vv in
          while !x > px + 1 do
            Buffer.add_char ops '=';
            decr x;
            decr y
          done;
          Buffer.add_char ops '-';
          decr x
        end
      done;
      while !x > 0 && !y > 0 do
        Buffer.add_char ops '=';
        decr x;
        decr y
      done;
      let s = Buffer.contents ops in
      let r =
        String.init (String.length s) (fun i -> s.[String.length s - 1 - i])
      in
      Printf.printf "%d\n%s" !found r
  | _ -> ()
