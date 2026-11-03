let max_level = 16

type skip_node = {
  val_data : int;
  forward : skip_node option array;
}

let make_node v lvl = {
  val_data = v;
  forward = Array.make (lvl + 1) None;
}

let seed = ref 42
let next_bit () =
  seed := (!seed * 1103515245 + 12345) land 0x7fffffff;
  !seed land 1

let random_level () =
  let lvl = ref 0 in
  while next_bit () = 1 && !lvl < max_level do
    incr lvl
  done;
  !lvl

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
    let head = make_node (-1) max_level in
    let current_level = ref 0 in

    let insert v =
      let update = Array.make (max_level + 1) head in
      let cur = ref head in
      for i = !current_level downto 0 do
        let cont = ref true in
        while !cont do
          match (!cur).forward.(i) with
          | Some next_node when next_node.val_data < v -> cur := next_node
          | _ -> cont := false
        done;
        update.(i) <- !cur
      done;
      let already =
        match (!cur).forward.(0) with
        | Some next_node -> next_node.val_data = v
        | None -> false
      in
      if not already then begin
        let rlevel = random_level () in
        if rlevel > !current_level then begin
          for i = !current_level + 1 to rlevel do
            update.(i) <- head
          done;
          current_level := rlevel
        end;
        let new_node = make_node v rlevel in
        for i = 0 to rlevel do
          new_node.forward.(i) <- update.(i).forward.(i);
          update.(i).forward.(i) <- Some new_node
        done
      end
    in

    let search v =
      let cur = ref head in
      for i = !current_level downto 0 do
        let cont = ref true in
        while !cont do
          match (!cur).forward.(i) with
          | Some next_node when next_node.val_data < v -> cur := next_node
          | _ -> cont := false
        done
      done;
      match (!cur).forward.(0) with
      | Some next_node -> next_node.val_data = v
      | None -> false
    in

    let remove v =
      let update = Array.make (max_level + 1) head in
      let cur = ref head in
      for i = !current_level downto 0 do
        let cont = ref true in
        while !cont do
          match (!cur).forward.(i) with
          | Some next_node when next_node.val_data < v -> cur := next_node
          | _ -> cont := false
        done;
        update.(i) <- !cur
      done;
      match (!cur).forward.(0) with
      | Some target when target.val_data = v ->
        let stop = ref false in
        let i = ref 0 in
        while !i <= !current_level && not !stop do
          match update.(!i).forward.(!i) with
          | Some node when node == target ->
            update.(!i).forward.(!i) <- target.forward.(!i);
            incr i
          | _ -> stop := true
        done;
        while !current_level > 0 && head.forward.(!current_level) = None do
          decr current_level
        done
      | _ -> ()
    in

    let get_all () =
      let res = ref [] in
      let cur = ref head.forward.(0) in
      while !cur <> None do
        match !cur with
        | Some node ->
          res := node.val_data :: !res;
          cur := node.forward.(0)
        | None -> ()
      done;
      List.rev !res
    in

    for _ = 0 to q - 1 do
      if !pos < Array.length tokens then begin
        let op = next_str () in
        if op = "INSERT" then begin
          let v = next_int () in
          insert v
        end else if op = "SEARCH" then begin
          let v = next_int () in
          if search v then print_endline "FOUND"
          else print_endline "NOT_FOUND"
        end else if op = "DELETE" then begin
          let v = next_int () in
          remove v
        end else if op = "PRINT" then begin
          let items = get_all () in
          let strs = List.map string_of_int items in
          print_endline (String.concat " " strs)
        end
      end
    done
  end
