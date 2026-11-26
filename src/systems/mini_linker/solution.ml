(* Mini static linker (CS:APP ch 7: resolution + relocation). *)

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

type raw_def = { dname : string; dsec : string; doff : int; dstrong : bool }
type reloc = { rsec : string; roff : int; rname : string; rabs : bool }
type obj = { ts : int; ds : int; defs : raw_def list; relocs : reloc list }

let () =
  match split_ws (read_all ()) with
  | [] -> ()
  | toks -> (
      let t = Array.of_list toks in
      let pos = ref 0 in
      let next () =
        let v = t.(!pos) in
        incr pos;
        v
      in
      let next_int () = int_of_string (next ()) in
      let m = next_int () in
      let objs =
        List.init m (fun _ ->
            let ts = next_int () in
            let ds = next_int () in
            let nd = next_int () in
            let dd =
              List.init nd (fun _ ->
                  let name = next () in
                  let sec = next () in
                  let off = next_int () in
                  let _size = next_int () in
                  let strong = next_int () = 1 in
                  { dname = name; dsec = sec; doff = off; dstrong = strong })
            in
            let nr = next_int () in
            let rr =
              List.init nr (fun _ ->
                  let sec = next () in
                  let off = next_int () in
                  let name = next () in
                  let is_abs = next () = "ABS" in
                  { rsec = sec; roff = off; rname = name; rabs = is_abs })
            in
            { ts; ds; defs = dd; relocs = rr })
      in
      let text_base = Array.make m 0 and data_base = Array.make m 0 in
      let addr = ref 0 in
      List.iteri
        (fun fi o ->
          text_base.(fi) <- !addr;
          addr := !addr + o.ts)
        objs;
      let text_size = !addr in
      List.iteri
        (fun fi o ->
          data_base.(fi) <- !addr;
          addr := !addr + o.ds)
        objs;
      let data_size = !addr - text_size in
      let defs = Hashtbl.create 16 in
      List.iteri
        (fun fi o ->
          List.iter
            (fun d ->
              let base =
                if d.dsec = "TEXT" then text_base.(fi) else data_base.(fi)
              in
              let prev = try Hashtbl.find defs d.dname with Not_found -> [] in
              Hashtbl.replace defs d.dname
                ((d.dstrong, fi, base + d.doff) :: prev))
            o.defs)
        objs;
      let names = Hashtbl.create 16 in
      Hashtbl.iter (fun k _ -> Hashtbl.replace names k true) defs;
      List.iter
        (fun o ->
          List.iter (fun r -> Hashtbl.replace names r.rname true) o.relocs)
        objs;
      let names =
        Hashtbl.fold (fun k _ acc -> k :: acc) names []
        |> List.sort String.compare
      in
      let syms = Hashtbl.create 16 in
      let err = ref None in
      List.iter
        (fun name ->
          if !err = None then
            match Hashtbl.find_opt defs name with
            | None -> err := Some ("ERROR undefined " ^ name)
            | Some ds ->
                let strong = List.filter (fun (s, _, _) -> s) ds in
                if List.length strong > 1 then
                  err := Some ("ERROR multiply-defined " ^ name)
                else
                  let a =
                    match strong with
                    | [ (_, _, a) ] -> a
                    | _ ->
                        List.fold_left
                          (fun best (_, fi, a) ->
                            match best with
                            | None -> Some (fi, a)
                            | Some (bf, _) ->
                                if fi < bf then Some (fi, a) else best)
                          None (List.rev ds)
                        |> Option.get |> snd
                  in
                  Hashtbl.replace syms name a)
        names;
      match !err with
      | Some e -> print_string e
      | None ->
          let out = Buffer.create 256 in
          Buffer.add_string out (Printf.sprintf "TEXT 0 %d\n" text_size);
          Buffer.add_string out
            (Printf.sprintf "DATA %d %d\n" text_size data_size);
          let sym_names =
            Hashtbl.fold (fun k _ acc -> k :: acc) syms []
            |> List.sort String.compare
          in
          List.iter
            (fun name ->
              Buffer.add_string out
                (Printf.sprintf "SYM %s %d\n" name (Hashtbl.find syms name)))
            sym_names;
          List.iteri
            (fun fi o ->
              List.iter
                (fun r ->
                  let base =
                    if r.rsec = "TEXT" then text_base.(fi) else data_base.(fi)
                  in
                  let place = base + r.roff in
                  let v =
                    if r.rabs then Hashtbl.find syms r.rname
                    else Hashtbl.find syms r.rname - (place + 4)
                  in
                  Buffer.add_string out
                    (Printf.sprintf "PATCH %d %s %d %d\n" fi r.rsec r.roff v))
                o.relocs)
            objs;
          print_string (Buffer.contents out))
