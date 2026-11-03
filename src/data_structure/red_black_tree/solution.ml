type color = Red | Black

type node = {
  mutable key : int64;
  mutable color : color;
  mutable left : int;
  mutable right : int;
  mutable p : int;
}

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
    let next_int64 () = Int64.of_string (next_str ()) in
    let q = next_int () in

    let cap = ref (max 32 (q + 2)) in
    let nodes = ref (Array.init !cap (fun _ -> { key = 0L; color = Black; left = 0; right = 0; p = 0 })) in
    let count = ref 1 in
    let root = ref 0 in

    let alloc_node k c p_idx =
      if !count >= !cap then begin
        let new_cap = !cap * 2 in
        let new_nodes = Array.init new_cap (fun i ->
          if i < !cap then (!nodes).(i)
          else { key = 0L; color = Black; left = 0; right = 0; p = 0 }
        ) in
        nodes := new_nodes;
        cap := new_cap
      end;
      let idx = !count in
      incr count;
      (!nodes).(idx).key <- k;
      (!nodes).(idx).color <- c;
      (!nodes).(idx).left <- 0;
      (!nodes).(idx).right <- 0;
      (!nodes).(idx).p <- p_idx;
      idx
    in

    let left_rotate x =
      let arr = !nodes in
      let y = arr.(x).right in
      arr.(x).right <- arr.(y).left;
      if arr.(y).left <> 0 then arr.(arr.(y).left).p <- x;
      arr.(y).p <- arr.(x).p;
      if arr.(x).p = 0 then root := y
      else if x = arr.(arr.(x).p).left then arr.(arr.(x).p).left <- y
      else arr.(arr.(x).p).right <- y;
      arr.(y).left <- x;
      arr.(x).p <- y
    in

    let right_rotate y =
      let arr = !nodes in
      let x = arr.(y).left in
      arr.(y).left <- arr.(x).right;
      if arr.(x).right <> 0 then arr.(arr.(x).right).p <- y;
      arr.(x).p <- arr.(y).p;
      if arr.(y).p = 0 then root := x
      else if y = arr.(arr.(y).p).right then arr.(arr.(y).p).right <- x
      else arr.(arr.(y).p).left <- x;
      arr.(x).right <- y;
      arr.(y).p <- x
    in

    let rb_insert_fixup z_init =
      let arr = !nodes in
      let z = ref z_init in
      while arr.(arr.(!z).p).color = Red do
        let zp = arr.(!z).p in
        let zpp = arr.(zp).p in
        if zp = arr.(zpp).left then begin
          let y = arr.(zpp).right in
          if arr.(y).color = Red then begin
            arr.(zp).color <- Black;
            arr.(y).color <- Black;
            arr.(zpp).color <- Red;
            z := zpp
          end else begin
            if !z = arr.(zp).right then begin
              z := zp;
              left_rotate !z
            end;
            let zp = arr.(!z).p in
            let zpp = arr.(zp).p in
            arr.(zp).color <- Black;
            arr.(zpp).color <- Red;
            right_rotate zpp
          end
        end else begin
          let y = arr.(zpp).left in
          if arr.(y).color = Red then begin
            arr.(zp).color <- Black;
            arr.(y).color <- Black;
            arr.(zpp).color <- Red;
            z := zpp
          end else begin
            if !z = arr.(zp).left then begin
              z := zp;
              right_rotate !z
            end;
            let zp = arr.(!z).p in
            let zpp = arr.(zp).p in
            arr.(zp).color <- Black;
            arr.(zpp).color <- Red;
            left_rotate zpp
          end
        end
      done;
      arr.(!root).color <- Black
    in

    let insert key =
      let arr = !nodes in
      let y = ref 0 in
      let x = ref !root in
      let duplicate = ref false in
      while !x <> 0 && not !duplicate do
        y := !x;
        let xk = arr.(!x).key in
        if key = xk then duplicate := true
        else if key < xk then x := arr.(!x).left
        else x := arr.(!x).right
      done;
      if not !duplicate then begin
        let z = alloc_node key Red !y in
        if !y = 0 then root := z
        else if key < (!nodes).(!y).key then (!nodes).(!y).left <- z
        else (!nodes).(!y).right <- z;
        rb_insert_fixup z
      end
    in

    let rb_transplant u v =
      let arr = !nodes in
      if arr.(u).p = 0 then root := v
      else if u = arr.(arr.(u).p).left then arr.(arr.(u).p).left <- v
      else arr.(arr.(u).p).right <- v;
      arr.(v).p <- arr.(u).p
    in

    let tree_minimum x_init =
      let arr = !nodes in
      let x = ref x_init in
      while arr.(!x).left <> 0 do
        x := arr.(!x).left
      done;
      !x
    in

    let rb_delete_fixup x_init =
      let arr = !nodes in
      let x = ref x_init in
      while !x <> !root && arr.(!x).color = Black do
        let xp = arr.(!x).p in
        if !x = arr.(xp).left then begin
          let w = ref arr.(xp).right in
          if arr.(!w).color = Red then begin
            arr.(!w).color <- Black;
            arr.(xp).color <- Red;
            left_rotate xp;
            w := arr.(arr.(!x).p).right
          end;
          let wl = arr.(!w).left in
          let wr = arr.(!w).right in
          if arr.(wl).color = Black && arr.(wr).color = Black then begin
            arr.(!w).color <- Red;
            x := arr.(!x).p
          end else begin
            if arr.(wr).color = Black then begin
              arr.(wl).color <- Black;
              arr.(!w).color <- Red;
              right_rotate !w;
              w := arr.(arr.(!x).p).right
            end;
            let xp = arr.(!x).p in
            arr.(!w).color <- arr.(xp).color;
            arr.(xp).color <- Black;
            arr.(arr.(!w).right).color <- Black;
            left_rotate xp;
            x := !root
          end
        end else begin
          let w = ref arr.(xp).left in
          if arr.(!w).color = Red then begin
            arr.(!w).color <- Black;
            arr.(xp).color <- Red;
            right_rotate xp;
            w := arr.(arr.(!x).p).left
          end;
          let wl = arr.(!w).left in
          let wr = arr.(!w).right in
          if arr.(wr).color = Black && arr.(wl).color = Black then begin
            arr.(!w).color <- Red;
            x := arr.(!x).p
          end else begin
            if arr.(wl).color = Black then begin
              arr.(wr).color <- Black;
              arr.(!w).color <- Red;
              left_rotate !w;
              w := arr.(arr.(!x).p).left
            end;
            let xp = arr.(!x).p in
            arr.(!w).color <- arr.(xp).color;
            arr.(xp).color <- Black;
            arr.(arr.(!w).left).color <- Black;
            right_rotate xp;
            x := !root
          end
        end
      done;
      arr.(!x).color <- Black
    in

    let find_node key =
      let arr = !nodes in
      let cur = ref !root in
      let found = ref 0 in
      while !cur <> 0 && !found = 0 do
        let ck = arr.(!cur).key in
        if key = ck then found := !cur
        else if key < ck then cur := arr.(!cur).left
        else cur := arr.(!cur).right
      done;
      !found
    in

    let remove key =
      let z = find_node key in
      if z <> 0 then begin
        let arr = !nodes in
        let y = ref z in
        let y_original_color = ref arr.(z).color in
        let x = ref 0 in
        let zl = arr.(z).left in
        let zr = arr.(z).right in
        if zl = 0 then begin
          x := zr;
          rb_transplant z zr
        end else if zr = 0 then begin
          x := zl;
          rb_transplant z zl
        end else begin
          y := tree_minimum zr;
          y_original_color := arr.(!y).color;
          x := arr.(!y).right;
          if arr.(!y).p = z then arr.(!x).p <- !y
          else begin
            rb_transplant !y arr.(!y).right;
            arr.(!y).right <- arr.(z).right;
            arr.(arr.(!y).right).p <- !y
          end;
          rb_transplant z !y;
          arr.(!y).left <- arr.(z).left;
          arr.(arr.(!y).left).p <- !y;
          arr.(!y).color <- arr.(z).color
        end;
        if !y_original_color = Black then rb_delete_fixup !x
      end
    in

    let predecessor key =
      let arr = !nodes in
      let cur = ref !root in
      let found = ref None in
      while !cur <> 0 do
        let ck = arr.(!cur).key in
        if ck < key then begin
          found := Some ck;
          cur := arr.(!cur).right
        end else
          cur := arr.(!cur).left
      done;
      !found
    in

    let successor key =
      let arr = !nodes in
      let cur = ref !root in
      let found = ref None in
      while !cur <> 0 do
        let ck = arr.(!cur).key in
        if ck > key then begin
          found := Some ck;
          cur := arr.(!cur).left
        end else
          cur := arr.(!cur).right
      done;
      !found
    in

    for _ = 0 to q - 1 do
      if !pos < Array.length tokens then begin
        let op = next_int () in
        let x = next_int64 () in
        if op = 1 then insert x
        else if op = 2 then remove x
        else if op = 3 then
          Printf.printf "%d\n" (if find_node x <> 0 then 1 else 0)
        else if op = 4 then
          match predecessor x with
          | Some v -> Printf.printf "%Ld\n" v
          | None -> print_endline "NONE"
        else if op = 5 then
          match successor x with
          | Some v -> Printf.printf "%Ld\n" v
          | None -> print_endline "NONE"
      end
    done
  end
