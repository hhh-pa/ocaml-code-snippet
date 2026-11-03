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

type einfo = { id : string; v1 : string; v2 : string }

let () =
  let tokens = read_all_tokens () in
  if Array.length tokens >= 2 then (
    let mode = tokens.(0) in
    let n = int_of_string tokens.(1) in
    let idx = ref 2 in

    let exprs = ref [] in
    let eidx = Hashtbl.create 16 in
    if mode = "AVAIL" then (
      let ne = int_of_string tokens.(!idx) in
      incr idx;
      for _ = 0 to ne - 1 do
        let eid = tokens.(!idx) in
        let v1 = tokens.(!idx + 1) in
        let _op = tokens.(!idx + 2) in
        let v2 = tokens.(!idx + 3) in
        idx := !idx + 4;
        Hashtbl.replace eidx eid (List.length !exprs);
        exprs := { id = eid; v1; v2 } :: !exprs
      done;
      exprs := List.rev !exprs
    );

    let succ = Array.make n [] in
    let defs = Array.make n [] in
    let uses = Array.make n [] in
    let gens = Array.make n [] in
    let allvars = ref [] in

    for b = 0 to n - 1 do
      let ns = int_of_string tokens.(!idx) in
      incr idx;
      let s_list = ref [] in
      for _ = 1 to ns do
        s_list := int_of_string tokens.(!idx) :: !s_list;
        incr idx
      done;
      succ.(b) <- List.rev !s_list;

      let nd = int_of_string tokens.(!idx) in
      incr idx;
      let d_list = ref [] in
      for _ = 1 to nd do
        let v = tokens.(!idx) in
        incr idx;
        d_list := v :: !d_list;
        allvars := v :: !allvars
      done;
      defs.(b) <- List.rev !d_list;

      if mode = "AVAIL" then (
        let ng = int_of_string tokens.(!idx) in
        incr idx;
        let g_list = ref [] in
        for _ = 1 to ng do
          g_list := tokens.(!idx) :: !g_list;
          incr idx
        done;
        gens.(b) <- List.rev !g_list
      ) else (
        let nu = int_of_string tokens.(!idx) in
        incr idx;
        let u_list = ref [] in
        for _ = 1 to nu do
          let v = tokens.(!idx) in
          incr idx;
          u_list := v :: !u_list;
          allvars := v :: !allvars
        done;
        uses.(b) <- List.rev !u_list
      )
    done;

    let pred = Array.make n [] in
    for b = 0 to n - 1 do
      List.iter (fun s -> pred.(s) <- b :: pred.(s)) succ.(b)
    done;
    for b = 0 to n - 1 do
      pred.(b) <- List.rev pred.(b)
    done;

    let def_mask = Array.make n 0L in
    let use_mask = Array.make n 0L in
    let gen_mask = Array.make n 0L in
    let kill_mask = Array.make n 0L in

    let names, u_count =
      if mode = "LIVE" then (
        let uniq = List.sort_uniq String.compare !allvars in
        let names_arr = Array.of_list uniq in
        let u = Array.length names_arr in
        let vi = Hashtbl.create u in
        Array.iteri (fun i name -> Hashtbl.replace vi name i) names_arr;
        for b = 0 to n - 1 do
          List.iter (fun v ->
            let i = Hashtbl.find vi v in
            def_mask.(b) <- Int64.logor def_mask.(b) (Int64.shift_left 1L i)
          ) defs.(b);
          List.iter (fun v ->
            let i = Hashtbl.find vi v in
            use_mask.(b) <- Int64.logor use_mask.(b) (Int64.shift_left 1L i)
          ) uses.(b)
        done;
        (names_arr, u)
      ) else if mode = "REACH" then (
        let dvar = ref [] in
        let dids = Array.make n [] in
        for b = 0 to n - 1 do
          let cur_dids = ref [] in
          List.iter (fun v ->
            let id = List.length !dvar in
            cur_dids := id :: !cur_dids;
            dvar := v :: !dvar
          ) defs.(b);
          dids.(b) <- List.rev !cur_dids
        done;
        let dvar_arr = Array.of_list (List.rev !dvar) in
        let u = Array.length dvar_arr in
        let names_arr = Array.init u (fun i -> "d" ^ string_of_int i) in
        for b = 0 to n - 1 do
          List.iter (fun id ->
            gen_mask.(b) <- Int64.logor gen_mask.(b) (Int64.shift_left 1L id)
          ) dids.(b);
          let mine_arr = Array.make u false in
          List.iter (fun id -> mine_arr.(id) <- true) dids.(b);
          for i = 0 to u - 1 do
            if not mine_arr.(i) then (
              if List.mem dvar_arr.(i) defs.(b) then
                kill_mask.(b) <- Int64.logor kill_mask.(b) (Int64.shift_left 1L i)
            )
          done
        done;
        (names_arr, u)
      ) else (
        let exprs_arr = Array.of_list !exprs in
        let u = Array.length exprs_arr in
        let names_arr = Array.init u (fun i -> exprs_arr.(i).id) in
        for b = 0 to n - 1 do
          List.iter (fun g ->
            let id = Hashtbl.find eidx g in
            gen_mask.(b) <- Int64.logor gen_mask.(b) (Int64.shift_left 1L id)
          ) gens.(b);
          for i = 0 to u - 1 do
            let e = exprs_arr.(i) in
            if List.mem e.v1 defs.(b) || List.mem e.v2 defs.(b) then
              kill_mask.(b) <- Int64.logor kill_mask.(b) (Int64.shift_left 1L i)
          done
        done;
        (names_arr, u)
      )
    in

    let all_mask =
      if u_count = 0 then 0L
      else if u_count = 64 then -1L
      else Int64.sub (Int64.shift_left 1L u_count) 1L
    in

    let in_mask = Array.make n 0L in
    let out_mask = Array.make n (if mode = "AVAIL" then all_mask else 0L) in

    let iters = ref 0 in
    if mode = "LIVE" then (
      let ch = ref true in
      while !ch do
        ch := false;
        incr iters;
        for b = n - 1 downto 0 do
          let o = ref 0L in
          List.iter (fun s -> o := Int64.logor !o in_mask.(s)) succ.(b);
          let o_val = !o in
          let ni = Int64.logand (Int64.logor use_mask.(b) (Int64.logand o_val (Int64.lognot def_mask.(b)))) all_mask in
          if o_val <> out_mask.(b) || ni <> in_mask.(b) then (
            out_mask.(b) <- o_val;
            in_mask.(b) <- ni;
            ch := true
          )
        done
      done
    ) else (
      let ch = ref true in
      while !ch do
        ch := false;
        incr iters;
        for b = 0 to n - 1 do
          let ni =
            if mode = "REACH" then (
              if b <> 0 then (
                let acc = ref 0L in
                List.iter (fun p -> acc := Int64.logor !acc out_mask.(p)) pred.(b);
                !acc
              ) else 0L
            ) else (
              if b = 0 then 0L
              else if pred.(b) = [] then all_mask
              else (
                let acc = ref all_mask in
                List.iter (fun p -> acc := Int64.logand !acc out_mask.(p)) pred.(b);
                !acc
              )
            )
          in
          let no = Int64.logand (Int64.logor gen_mask.(b) (Int64.logand ni (Int64.lognot kill_mask.(b)))) all_mask in
          if ni <> in_mask.(b) || no <> out_mask.(b) then (
            in_mask.(b) <- ni;
            out_mask.(b) <- no;
            ch := true
          )
        done
      done
    );

    for b = 0 to n - 1 do
      Printf.printf "B%d IN={" b;
      let first = ref true in
      for i = 0 to u_count - 1 do
        if Int64.logand (Int64.shift_right_logical in_mask.(b) i) 1L = 1L then (
          if not !first then print_char ',';
          print_string names.(i);
          first := false
        )
      done;
      print_string "} OUT={";
      let first2 = ref true in
      for i = 0 to u_count - 1 do
        if Int64.logand (Int64.shift_right_logical out_mask.(b) i) 1L = 1L then (
          if not !first2 then print_char ',';
          print_string names.(i);
          first2 := false
        )
      done;
      print_string "}\n"
    done;
    Printf.printf "ITERATIONS %d\n" !iters
  )
