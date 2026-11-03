let read_all_tokens () =
  let content =
    let b = Buffer.create 4096 in
    let buf = Bytes.create 4096 in
    let rec loop () =
      let n = input stdin buf 0 4096 in
      if n > 0 then (
        Buffer.add_subbytes b buf 0 n;
        loop ()
      )
    in
    loop ();
    Buffer.contents b
  in
  let len = String.length content in
  let tokens = ref [] in
  let i = ref 0 in
  while !i < len do
    while !i < len && (let c = content.[!i] in c = ' ' || c = '\t' || c = '\r' || c = '\n') do
      incr i
    done;
    if !i < len then (
      let start = !i in
      while !i < len && (let c = content.[!i] in c <> ' ' && c <> '\t' && c <> '\r' && c <> '\n') do
        incr i
      done;
      tokens := String.sub content start (!i - start) :: !tokens
    )
  done;
  Array.of_list (List.rev !tokens)

type instruction = {
  dest : string;
  opcode : string;
  arg1 : string;
  arg2 : string;
}

let () =
  let tokens = read_all_tokens () in
  if Array.length tokens >= 1 then (
    let n_inst = int_of_string tokens.(0) in
    let idx = ref 1 in
    let insts = ref [] in
    for _ = 1 to n_inst do
      if !idx + 2 < Array.length tokens then (
        let dest = tokens.(!idx) in
        let _eq = tokens.(!idx + 1) in
        let opcode = tokens.(!idx + 2) in
        idx := !idx + 3;
        if opcode = "CONST" then (
          let val_str = tokens.(!idx) in
          incr idx;
          insts := { dest; opcode; arg1 = val_str; arg2 = "" } :: !insts
        ) else if opcode = "COPY" then (
          let src = tokens.(!idx) in
          incr idx;
          insts := { dest; opcode; arg1 = src; arg2 = "" } :: !insts
        ) else (
          let a1 = tokens.(!idx) in
          let a2 = tokens.(!idx + 1) in
          idx := !idx + 2;
          insts := { dest; opcode; arg1 = a1; arg2 = a2 } :: !insts
        )
      )
    done;
    let insts = Array.of_list (List.rev !insts) in

    let var_to_vn = Hashtbl.create 32 in
    let vn_to_rep = Hashtbl.create 32 in
    let expr_to_vn = Hashtbl.create 32 in
    let const_to_vn = Hashtbl.create 32 in

    let next_vn = ref 1 in
    let redundant_count = ref 0 in

    let get_var_vn v =
      match Hashtbl.find_opt var_to_vn v with
      | Some vn -> vn
      | None ->
        let vn = !next_vn in
        incr next_vn;
        Hashtbl.replace var_to_vn v vn;
        Hashtbl.replace vn_to_rep vn v;
        vn
    in

    Array.iter (fun inst ->
      if inst.opcode = "CONST" then (
        let v_val = Int64.of_string inst.arg1 in
        match Hashtbl.find_opt const_to_vn v_val with
        | Some vn ->
          Hashtbl.replace var_to_vn inst.dest vn;
          incr redundant_count;
          let rep = Hashtbl.find vn_to_rep vn in
          Printf.printf "%s = %s (VN%d REDUNDANT)\n" inst.dest rep vn
        | None ->
          let vn = !next_vn in
          incr next_vn;
          Hashtbl.replace const_to_vn v_val vn;
          Hashtbl.replace var_to_vn inst.dest vn;
          Hashtbl.replace vn_to_rep vn inst.dest;
          Printf.printf "%s = CONST %s (VN%d)\n" inst.dest inst.arg1 vn
      ) else if inst.opcode = "COPY" then (
        let src_vn = get_var_vn inst.arg1 in
        Hashtbl.replace var_to_vn inst.dest src_vn;
        incr redundant_count;
        let rep = Hashtbl.find vn_to_rep src_vn in
        Printf.printf "%s = %s (VN%d REDUNDANT)\n" inst.dest rep src_vn
      ) else if inst.opcode = "PHI" then (
        let vn1 = get_var_vn inst.arg1 in
        let vn2 = get_var_vn inst.arg2 in
        if vn1 = vn2 then (
          Hashtbl.replace var_to_vn inst.dest vn1;
          incr redundant_count;
          let rep = Hashtbl.find vn_to_rep vn1 in
          Printf.printf "%s = %s (VN%d REDUNDANT PHI)\n" inst.dest rep vn1
        ) else (
          let c1, c2 = if vn1 > vn2 then (vn2, vn1) else (vn1, vn2) in
          let expr_key = ("PHI", c1, c2) in
          match Hashtbl.find_opt expr_to_vn expr_key with
          | Some vn ->
            Hashtbl.replace var_to_vn inst.dest vn;
            incr redundant_count;
            let rep = Hashtbl.find vn_to_rep vn in
            Printf.printf "%s = %s (VN%d REDUNDANT)\n" inst.dest rep vn
          | None ->
            let vn = !next_vn in
            incr next_vn;
            Hashtbl.replace expr_to_vn expr_key vn;
            Hashtbl.replace var_to_vn inst.dest vn;
            Hashtbl.replace vn_to_rep vn inst.dest;
            Printf.printf "%s = PHI VN%d VN%d (VN%d)\n" inst.dest c1 c2 vn
        )
      ) else (
        let vn1 = get_var_vn inst.arg1 in
        let vn2 = get_var_vn inst.arg2 in
        let op = inst.opcode in
        let c1, c2 =
          if (op = "ADD" || op = "MUL") && vn1 > vn2 then (vn2, vn1) else (vn1, vn2)
        in
        let expr_key = (op, c1, c2) in
        match Hashtbl.find_opt expr_to_vn expr_key with
        | Some vn ->
          Hashtbl.replace var_to_vn inst.dest vn;
          incr redundant_count;
          let rep = Hashtbl.find vn_to_rep vn in
          Printf.printf "%s = %s (VN%d REDUNDANT)\n" inst.dest rep vn
        | None ->
          let vn = !next_vn in
          incr next_vn;
          Hashtbl.replace expr_to_vn expr_key vn;
          Hashtbl.replace var_to_vn inst.dest vn;
          Hashtbl.replace vn_to_rep vn inst.dest;
          Printf.printf "%s = %s VN%d VN%d (VN%d)\n" inst.dest op c1 c2 vn
      )
    ) insts;

    Printf.printf "Total unique VNs: %d\n" (!next_vn - 1);
    Printf.printf "Redundant instructions eliminated: %d\n" !redundant_count
  )
