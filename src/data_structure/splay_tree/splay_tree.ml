let () =
  let tokens =
    let buf = Buffer.create 4096 in
    (try while true do Buffer.add_channel buf stdin 4096 done with End_of_file -> ());
    let s = Buffer.contents buf in
    let len = String.length s in
    let res = ref [] in
    let i = ref 0 in
    while !i < len do
      while !i < len && (let c = s.[!i] in c = ' ' || c = '\t' || c = '\r' || c = '\n') do incr i done;
      if !i < len then begin
        let st = !i in
        while !i < len && (let c = s.[!i] in c <> ' ' && c <> '\t' && c <> '\r' && c <> '\n') do incr i done;
        res := String.sub s st (!i - st) :: !res
      end
    done;
    Array.of_list (List.rev !res)
  in
  if Array.length tokens >= 1 then begin
    let pos = ref 0 in
    let next_str () = let t = tokens.(!pos) in incr pos; t in
    let next_int () = int_of_string (next_str ()) in
    let q = next_int () in
    let cap = ref (max 32 (q + 2)) in
    let keys = ref (Array.make !cap 0) in
    let left = ref (Array.make !cap 0) in
    let right = ref (Array.make !cap 0) in
    let parent = ref (Array.make !cap 0) in
    let count = ref 1 in
    let root = ref 0 in

    let alloc_node k =
      if !count >= !cap then begin
        let new_cap = !cap * 2 in
        let grow arr =
          let na = Array.make new_cap 0 in
          Array.blit !arr 0 na 0 !cap;
          arr := na
        in
        grow keys;
        grow left;
        grow right;
        grow parent;
        cap := new_cap
      end;
      let idx = !count in
      incr count;
      (!keys).(idx) <- k;
      (!left).(idx) <- 0;
      (!right).(idx) <- 0;
      (!parent).(idx) <- 0;
      idx
    in

    let rotate_left x =
      let r = !right in
      let l = !left in
      let p = !parent in
      let y = r.(x) in
      if y <> 0 then begin
        r.(x) <- l.(y);
        if l.(y) <> 0 then p.(l.(y)) <- x;
        p.(y) <- p.(x);
        if p.(x) = 0 then root := y
        else if x = l.(p.(x)) then l.(p.(x)) <- y
        else r.(p.(x)) <- y;
        l.(y) <- x;
        p.(x) <- y
      end
    in

    let rotate_right x =
      let r = !right in
      let l = !left in
      let p = !parent in
      let y = l.(x) in
      if y <> 0 then begin
        l.(x) <- r.(y);
        if r.(y) <> 0 then p.(r.(y)) <- x;
        p.(y) <- p.(x);
        if p.(x) = 0 then root := y
        else if x = l.(p.(x)) then l.(p.(x)) <- y
        else r.(p.(x)) <- y;
        r.(y) <- x;
        p.(x) <- y
      end
    in

    let splay x =
      let p = !parent in
      let l = !left in
      let r = !right in
      while p.(x) <> 0 do
        let px = p.(x) in
        let g = p.(px) in
        if g = 0 then begin
          if x = l.(px) then rotate_right px
          else rotate_left px
        end else if x = l.(px) && px = l.(g) then begin
          rotate_right g;
          rotate_right px
        end else if x = r.(px) && px = r.(g) then begin
          rotate_left g;
          rotate_left px
        end else if x = r.(px) && px = l.(g) then begin
          rotate_left px;
          rotate_right g
        end else begin
          rotate_right px;
          rotate_left g
        end
      done
    in

    let insert key =
      if !root = 0 then begin
        root := alloc_node key
      end else begin
        let cur = ref !root in
        let p_node = ref 0 in
        let found = ref false in
        while !cur <> 0 && not !found do
          p_node := !cur;
          let ck = (!keys).(!cur) in
          if key < ck then cur := (!left).(!cur)
          else if key > ck then cur := (!right).(!cur)
          else begin
            splay !cur;
            found := true
          end
        done;
        if not !found then begin
          let node = alloc_node key in
          (!parent).(node) <- !p_node;
          if key < (!keys).(!p_node) then (!left).(!p_node) <- node
          else (!right).(!p_node) <- node;
          splay node
        end
      end
    in

    let find key =
      let cur = ref !root in
      let last = ref 0 in
      let found = ref false in
      while !cur <> 0 && not !found do
        last := !cur;
        let ck = (!keys).(!cur) in
        if key < ck then cur := (!left).(!cur)
        else if key > ck then cur := (!right).(!cur)
        else begin
          splay !cur;
          found := true
        end
      done;
      if not !found && !last <> 0 then splay !last;
      !found
    in

    let remove key =
      if find key then begin
        let left_sub = (!left).(!root) in
        let right_sub = (!right).(!root) in
        if left_sub <> 0 then (!parent).(left_sub) <- 0;
        if right_sub <> 0 then (!parent).(right_sub) <- 0;
        if left_sub = 0 then root := right_sub
        else begin
          let m = ref left_sub in
          while (!right).(!m) <> 0 do
            m := (!right).(!m)
          done;
          root := left_sub;
          splay !m;
          (!right).(!root) <- right_sub;
          if right_sub <> 0 then (!parent).(right_sub) <- !root
        end
      end
    in

    let get_inorder () =
      let res = ref [] in
      let rec walk u =
        if u <> 0 then begin
          walk (!left).(u);
          res := (!keys).(u) :: !res;
          walk (!right).(u)
        end
      in
      walk !root;
      List.rev !res
    in

    for _ = 0 to q - 1 do
      if !pos < Array.length tokens then begin
        let op = next_str () in
        if op = "INSERT" then begin
          let v = next_int () in
          insert v
        end else if op = "FIND" then begin
          let v = next_int () in
          if find v then print_endline "FOUND"
          else print_endline "NOT_FOUND"
        end else if op = "DELETE" then begin
          let v = next_int () in
          remove v
        end else if op = "PRINT" then begin
          let items = get_inorder () in
          let strs = List.map string_of_int items in
          print_endline (String.concat " " strs)
        end
      end
    done
  end
