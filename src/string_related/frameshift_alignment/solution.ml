(* Frameshift-aware DNA/protein alignment (string-to-network matching). *)

let codons = function
  | 'F' -> [ "TTT"; "TTC" ]
  | 'L' -> [ "TTA"; "TTG"; "CTT"; "CTC"; "CTA"; "CTG" ]
  | 'I' -> [ "ATT"; "ATC"; "ATA" ]
  | 'M' -> [ "ATG" ]
  | 'V' -> [ "GTT"; "GTC"; "GTA"; "GTG" ]
  | 'S' -> [ "TCT"; "TCC"; "TCA"; "TCG"; "AGT"; "AGC" ]
  | 'P' -> [ "CCT"; "CCC"; "CCA"; "CCG" ]
  | 'T' -> [ "ACT"; "ACC"; "ACA"; "ACG" ]
  | 'A' -> [ "GCT"; "GCC"; "GCA"; "GCG" ]
  | 'Y' -> [ "TAT"; "TAC" ]
  | 'H' -> [ "CAT"; "CAC" ]
  | 'Q' -> [ "CAA"; "CAG" ]
  | 'N' -> [ "AAT"; "AAC" ]
  | 'K' -> [ "AAA"; "AAG" ]
  | 'D' -> [ "GAT"; "GAC" ]
  | 'E' -> [ "GAA"; "GAG" ]
  | 'C' -> [ "TGT"; "TGC" ]
  | 'W' -> [ "TGG" ]
  | 'R' -> [ "CGT"; "CGC"; "CGA"; "CGG"; "AGA"; "AGG" ]
  | 'G' -> [ "GGT"; "GGC"; "GGA"; "GGG" ]
  | _ -> []

let inf = 4000000000000000000L

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
  | _ :: subs :: inss :: dels :: s :: prot :: _ ->
      let sub = Int64.of_string subs
      and ins = Int64.of_string inss
      and dele = Int64.of_string dels in
      let edges = ref [] and nid = ref 0 and entry = ref 0 in
      String.iter
        (fun aa ->
          incr nid;
          let nex = !nid in
          List.iter
            (fun cod ->
              let u = ref !entry in
              String.iter
                (fun ch ->
                  incr nid;
                  edges := (!u, !nid, ch) :: !edges;
                  u := !nid)
                cod;
              let last = !u in
              if last <> nex then begin
                edges :=
                  List.map
                    (fun (a, b, c) ->
                      ( (if a = last then nex else a),
                        (if b = last then nex else b),
                        c ))
                    !edges;
                decr nid
              end)
            (List.sort compare (codons aa));
          entry := nex)
        prot;
      let nnodes = !nid + 1 and t = !entry in
      let into = Array.make nnodes [] in
      List.iter (fun (u, v, c) -> into.(v) <- (u, c) :: into.(v)) !edges;
      for v = 0 to nnodes - 1 do
        into.(v) <-
          List.sort
            (fun (u1, c1) (u2, c2) ->
              if c1 <> c2 then compare c1 c2 else compare u1 u2)
            into.(v)
      done;
      let n = String.length s in
      let dist = Array.init (n + 1) (fun _ -> Array.make nnodes inf) in
      let pk = Array.init (n + 1) (fun _ -> Array.make nnodes 0) in
      let pu = Array.init (n + 1) (fun _ -> Array.make nnodes 0) in
      let pc = Array.init (n + 1) (fun _ -> Array.make nnodes ' ') in
      dist.(0).(0) <- 0L;
      for i = 1 to n do
        dist.(i).(0) <- Int64.mul (Int64.of_int i) dele;
        pk.(i).(0) <- 1
      done;
      for v = 1 to nnodes - 1 do
        let best = ref inf and bu = ref 0 and bc = ref ' ' in
        List.iter
          (fun (u, c) ->
            let cand = Int64.add dist.(0).(u) ins in
            if cand < !best then begin
              best := cand;
              bu := u;
              bc := c
            end)
          into.(v);
        dist.(0).(v) <- !best;
        pk.(0).(v) <- 2;
        pu.(0).(v) <- !bu;
        pc.(0).(v) <- !bc
      done;
      for i = 1 to n do
        for v = 1 to nnodes - 1 do
          let best = ref inf and bk = ref 0 and bu = ref 0 and bc = ref ' ' in
          List.iter
            (fun (u, c) ->
              let cs =
                Int64.add dist.(i - 1).(u) (if s.[i - 1] = c then 0L else sub)
              in
              if cs < !best then begin
                best := cs;
                bk := 3;
                bu := u;
                bc := c
              end)
            into.(v);
          if Int64.add dist.(i - 1).(v) dele < !best then begin
            best := Int64.add dist.(i - 1).(v) dele;
            bk := 1
          end;
          List.iter
            (fun (u, c) ->
              let ci = Int64.add dist.(i).(u) ins in
              if ci < !best then begin
                best := ci;
                bk := 2;
                bu := u;
                bc := c
              end)
            into.(v);
          dist.(i).(v) <- !best;
          pk.(i).(v) <- !bk;
          pu.(i).(v) <- !bu;
          pc.(i).(v) <- !bc
        done
      done;
      let out = Buffer.create 16 in
      let i = ref n and v = ref t in
      while not (!i = 0 && !v = 0) do
        match pk.(!i).(!v) with
        | 1 -> decr i
        | 2 ->
            Buffer.add_char out pc.(!i).(!v);
            v := pu.(!i).(!v)
        | _ ->
            Buffer.add_char out pc.(!i).(!v);
            let u = pu.(!i).(!v) in
            decr i;
            v := u
      done;
      let r = Buffer.contents out in
      let rr =
        String.init (String.length r) (fun k -> r.[String.length r - 1 - k])
      in
      Printf.printf "%Ld\n%s" dist.(n).(t) rr
  | _ -> ()
