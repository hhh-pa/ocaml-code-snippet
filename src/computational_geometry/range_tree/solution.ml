type pt = { x : int64; y : int64 }

type range_node = {
  l : int;
  r : int;
  mutable mid : int;
  mutable left : int;
  mutable right : int;
  mutable yvec : int array;
  mutable fcl : int array;
  mutable fcr : int array;
}

let cmp_pt a b =
  let c = Int64.compare a.x b.x in
  if c <> 0 then c else Int64.compare a.y b.y

let inf_y = 4000000000000000000L

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
    let n = next_int () in
    let pts = Array.init n (fun _ ->
      let x = next_int64 () in
      let y = next_int64 () in
      { x; y }
    ) in
    let xs = Array.init n (fun i -> i) in
    Array.sort (fun a b -> cmp_pt pts.(a) pts.(b)) xs;

    let tree = ref [] in

    let rec build l r =
      let node_id = List.length !tree in
      let nd = {
        l;
        r;
        mid = -1;
        left = -1;
        right = -1;
        yvec = [||];
        fcl = [||];
        fcr = [||];
      } in
      tree := nd :: !tree;

      let yv = Array.init (r - l) (fun i -> xs.(l + i)) in
      Array.sort (fun a b ->
        let cy = Int64.compare pts.(a).y pts.(b).y in
        if cy <> 0 then cy else Int64.compare pts.(a).x pts.(b).x
      ) yv;
      nd.yvec <- yv;

      if r - l = 1 then begin
        nd.mid <- l;
        nd.left <- -1;
        nd.right <- -1;
        node_id
      end else begin
        let mid = (l + r) / 2 in
        nd.mid <- mid;
        let left_id = build l mid in
        let right_id = build mid r in
        nd.left <- left_id;
        nd.right <- right_id;
        let tree_arr = Array.of_list (List.rev !tree) in
        let ly = tree_arr.(left_id).yvec in
        let ry = tree_arr.(right_id).yvec in
        let ys = Array.length yv in
        let fcl = Array.make (ys + 1) 0 in
        let fcr = Array.make (ys + 1) 0 in
        for i = 0 to ys do
          let yb = if i < ys then pts.(yv.(i)).y else inf_y in
          let lo = ref 0 in
          let hi = ref (Array.length ly) in
          while !lo < !hi do
            let mm = (!lo + !hi) / 2 in
            if pts.(ly.(mm)).y < yb then lo := mm + 1 else hi := mm
          done;
          fcl.(i) <- !lo;
          lo := 0;
          hi := Array.length ry;
          while !lo < !hi do
            let mm = (!lo + !hi) / 2 in
            if pts.(ry.(mm)).y < yb then lo := mm + 1 else hi := mm
          done;
          fcr.(i) <- !hi
        done;
        nd.fcl <- fcl;
        nd.fcr <- fcr;
        node_id
      end
    in

    let root = if n > 0 then build 0 n else -1 in
    let tree_arr = Array.of_list (List.rev !tree) in

    if !pos < Array.length tokens then begin
      let q = next_int () in
      for _ = 0 to q - 1 do
        if !pos < Array.length tokens then begin
          let x1 = next_int64 () in
          let y1 = next_int64 () in
          let x2 = next_int64 () in
          let y2 = next_int64 () in
          let found = ref [] in

          let v = ref root in
          let stop = ref false in
          while tree_arr.(!v).left <> -1 && not !stop do
            let key = pts.(xs.(tree_arr.(!v).mid)).x in
            if x2 < key then v := tree_arr.(!v).left
            else if x1 > key then v := tree_arr.(!v).right
            else stop := true
          done;

          if tree_arr.(!v).left = -1 then begin
            let pid = tree_arr.(!v).yvec.(0) in
            if x1 <= pts.(pid).x && pts.(pid).x <= x2 && y1 <= pts.(pid).y && pts.(pid).y <= y2 then
              found := [pid]
          end else begin
            let syv = tree_arr.(!v).yvec in
            let lo = ref 0 in
            let hi = ref (Array.length syv) in
            while !lo < !hi do
              let mm = (!lo + !hi) / 2 in
              if pts.(syv.(mm)).y < y1 then lo := mm + 1 else hi := mm
            done;
            let lo0 = !lo in
            lo := 0;
            hi := Array.length syv;
            let y2p = if y2 = Int64.sub inf_y 1L then inf_y else Int64.add y2 1L in
            while !lo < !hi do
              let mm = (!lo + !hi) / 2 in
              if pts.(syv.(mm)).y < y2p then lo := mm + 1 else hi := mm
            done;
            let hi0 = !lo in

            let lv = tree_arr.(!v).left in
            let rv = tree_arr.(!v).right in
            let llo = tree_arr.(!v).fcl.(lo0) in
            let lhi = tree_arr.(!v).fcl.(hi0) in
            let rlo = tree_arr.(!v).fcr.(lo0) in
            let rhi = tree_arr.(!v).fcr.(hi0) in

            (* left walk *)
            let u = ref lv in
            let a = ref llo in
            let b = ref lhi in
            let stop_lw = ref false in
            while not !stop_lw do
              if tree_arr.(!u).left = -1 then begin
                if !b > !a then begin
                  let pid = tree_arr.(!u).yvec.(0) in
                  if x1 <= pts.(pid).x then found := pid :: !found
                end;
                stop_lw := true
              end else begin
                let key = pts.(xs.(tree_arr.(!u).mid)).x in
                if x1 <= key then begin
                  let c = tree_arr.(!u).right in
                  for i = tree_arr.(!u).fcr.(!a) to tree_arr.(!u).fcr.(!b) - 1 do
                    found := tree_arr.(c).yvec.(i) :: !found
                  done;
                  let nl = tree_arr.(!u).left in
                  let na = tree_arr.(!u).fcl.(!a) in
                  let nb = tree_arr.(!u).fcl.(!b) in
                  u := nl; a := na; b := nb
                end else begin
                  let nr = tree_arr.(!u).right in
                  let na = tree_arr.(!u).fcr.(!a) in
                  let nb = tree_arr.(!u).fcr.(!b) in
                  u := nr; a := na; b := nb
                end
              end
            done;

            (* right walk *)
            u := rv;
            a := rlo;
            b := rhi;
            let stop_rw = ref false in
            while not !stop_rw do
              if tree_arr.(!u).left = -1 then begin
                if !b > !a then begin
                  let pid = tree_arr.(!u).yvec.(0) in
                  if pts.(pid).x <= x2 then found := pid :: !found
                end;
                stop_rw := true
              end else begin
                let key = pts.(xs.(tree_arr.(!u).mid)).x in
                if key <= x2 then begin
                  let c = tree_arr.(!u).left in
                  for i = tree_arr.(!u).fcl.(!a) to tree_arr.(!u).fcl.(!b) - 1 do
                    found := tree_arr.(c).yvec.(i) :: !found
                  done;
                  let nr2 = tree_arr.(!u).right in
                  let na = tree_arr.(!u).fcr.(!a) in
                  let nb = tree_arr.(!u).fcr.(!b) in
                  u := nr2; a := na; b := nb
                end else begin
                  let nl = tree_arr.(!u).left in
                  let na = tree_arr.(!u).fcl.(!a) in
                  let nb = tree_arr.(!u).fcl.(!b) in
                  u := nl; a := na; b := nb
                end
              end
            done
          end;

          let found_arr = Array.of_list !found in
          Array.sort (fun a b -> cmp_pt pts.(a) pts.(b)) found_arr;
          let flen = Array.length found_arr in
          Printf.printf "%d\n" flen;
          if flen > 0 then begin
            for k = 0 to flen - 1 do
              if k > 0 then print_string " ";
              Printf.printf "%Ld,%Ld" pts.(found_arr.(k)).x pts.(found_arr.(k)).y
            done;
            print_newline ()
          end
        end
      done
    end
  end
