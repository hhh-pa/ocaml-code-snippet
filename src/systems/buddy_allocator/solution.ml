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

module IntSet = Set.Make(Int)

let () =
  let tokens = read_all_tokens () in
  if Array.length tokens >= 2 then (
    let u = int_of_string tokens.(0) in
    let q = int_of_string tokens.(1) in
    let idx = ref 2 in
    let free_list = Array.make (u + 1) IntSet.empty in
    free_list.(u) <- IntSet.singleton 0;
    let alloc = Hashtbl.create 64 in

    for _ = 0 to q - 1 do
      let op = int_of_string tokens.(!idx) in
      incr idx;
      if op = 1 then (
        let s = int_of_string tokens.(!idx) in
        incr idx;
        if s < 1 then print_endline "FAIL"
        else (
          let need = ref 0 in
          while !need <= u && (1 lsl !need) < s do
            incr need
          done;
          let n_req = !need in
          let found = ref (-1) in
          let at = ref 0 in
          let o = ref n_req in
          while !o <= u && !found = -1 do
            if not (IntSet.is_empty free_list.(!o)) then (
              found := !o;
              at := IntSet.min_elt free_list.(!o)
            );
            incr o
          done;
          if n_req > u || !found = -1 then print_endline "FAIL"
          else (
            let f = !found in
            let a = !at in
            free_list.(f) <- IntSet.remove a free_list.(f);
            for split_o = f downto n_req + 1 do
              let half = 1 lsl (split_o - 1) in
              free_list.(split_o - 1) <- IntSet.add (a + half) free_list.(split_o - 1)
            done;
            Hashtbl.replace alloc a n_req;
            Printf.printf "%d\n" a
          )
        )
      ) else if op = 2 then (
        let a = int_of_string tokens.(!idx) in
        incr idx;
        if not (Hashtbl.mem alloc a) then print_endline "FAIL"
        else (
          let o = ref (Hashtbl.find alloc a) in
          Hashtbl.remove alloc a;
          let at = ref a in
          let merging = ref true in
          while !o < u && !merging do
            let buddy = !at lxor (1 lsl !o) in
            if not (IntSet.mem buddy free_list.(!o)) then
              merging := false
            else (
              free_list.(!o) <- IntSet.remove buddy free_list.(!o);
              if buddy < !at then at := buddy;
              incr o
            )
          done;
          free_list.(!o) <- IntSet.add !at free_list.(!o);
          print_endline "OK"
        )
      ) else if op = 3 then (
        let counts = Array.init (u + 1) (fun o_idx ->
          string_of_int (IntSet.cardinal free_list.(o_idx))
        ) in
        print_endline (String.concat " " (Array.to_list counts))
      )
    done
  )
