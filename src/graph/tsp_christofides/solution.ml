(* Christofides 3/2 TSP: MST + subset-DP matching + Euler + shortcut. *)

let cf_find parent a =
  let x = ref a in
  while parent.(!x) <> !x do
    parent.(!x) <- parent.(parent.(!x));
    x := parent.(!x)
  done;
  !x

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
  | [] -> ()
  | toks ->
      let t = Array.of_list toks in
      let n = int_of_string t.(0) in
      let kind = t.(1) in
      let w = Array.make_matrix n n 0.0 in
      if kind = "LINE" then begin
        let xs = Array.init n (fun i -> float_of_string t.(2 + i)) in
        for i = 0 to n - 1 do
          for j = 0 to n - 1 do
            w.(i).(j) <- abs_float (xs.(i) -. xs.(j))
          done
        done
      end
      else begin
        let px = Array.init n (fun i -> float_of_string t.(2 + (2 * i))) in
        let py = Array.init n (fun i -> float_of_string t.(3 + (2 * i))) in
        for i = 0 to n - 1 do
          for j = 0 to n - 1 do
            let dx = px.(i) -. px.(j) and dy = py.(i) -. py.(j) in
            w.(i).(j) <- sqrt ((dx *. dx) +. (dy *. dy))
          done
        done
      end;
      let edges = ref [] in
      for i = 0 to n - 1 do
        for j = i + 1 to n - 1 do
          edges := (w.(i).(j), i, j) :: !edges
        done
      done;
      let edges = List.sort compare !edges in
      let parent = Array.init n (fun i -> i) in
      let mst = ref [] in
      List.iter
        (fun (_, u, v) ->
          let ru = cf_find parent u and rv = cf_find parent v in
          if ru <> rv then begin
            parent.(ru) <- rv;
            mst := (u, v) :: !mst
          end)
        edges;
      let deg = Array.make n 0 in
      List.iter
        (fun (u, v) ->
          deg.(u) <- deg.(u) + 1;
          deg.(v) <- deg.(v) + 1)
        !mst;
      let odd = ref [] in
      for i = 0 to n - 1 do
        if deg.(i) mod 2 = 1 then odd := i :: !odd
      done;
      let odd = Array.of_list (List.rev !odd) in
      let k = Array.length odd in
      let dp = Array.make (1 lsl k) infinity in
      let choice = Array.make (1 lsl k) 0 in
      dp.(0) <- 0.0;
      for mask = 1 to (1 lsl k) - 1 do
        let i = ref 0 in
        while mask land (1 lsl !i) = 0 do
          incr i
        done;
        let rest = mask lxor (1 lsl !i) in
        let best = ref infinity and bj = ref 0 in
        for j = !i + 1 to k - 1 do
          if rest land (1 lsl j) <> 0 then
            let c = w.(odd.(!i)).(odd.(j)) +. dp.(rest lxor (1 lsl j)) in
            if c < !best then begin
              best := c;
              bj := j
            end
        done;
        dp.(mask) <- !best;
        choice.(mask) <- !bj
      done;
      let adj = Array.make n [] in
      let ne = ref 0 in
      let add_edge u v =
        adj.(u) <- (v, !ne) :: adj.(u);
        adj.(v) <- (u, !ne) :: adj.(v);
        incr ne
      in
      List.iter (fun (u, v) -> add_edge u v) !mst;
      let mask = ref ((1 lsl k) - 1) in
      while !mask <> 0 do
        let i = ref 0 in
        while !mask land (1 lsl !i) = 0 do
          incr i
        done;
        let j = choice.(!mask) in
        add_edge odd.(!i) odd.(j);
        mask := !mask lxor (1 lsl !i) lxor (1 lsl j)
      done;
      for i = 0 to n - 1 do
        adj.(i) <- List.sort (fun a b -> compare b a) adj.(i)
      done;
      let used = Array.make !ne false in
      let stack = ref [ 0 ] and tour = ref [] in
      while !stack <> [] do
        let v = List.hd !stack in
        let rec clean () =
          match adj.(v) with
          | (_, e) :: rest when used.(e) ->
              adj.(v) <- rest;
              clean ()
          | _ -> ()
        in
        clean ();
        match adj.(v) with
        | [] ->
            tour := v :: !tour;
            stack := List.tl !stack
        | (u, e) :: rest ->
            adj.(v) <- rest;
            used.(e) <- true;
            stack := u :: !stack
      done;
      let tour = List.rev !tour in
      let seen = Array.make n false in
      let sht = ref [] in
      List.iter
        (fun v ->
          if not seen.(v) then begin
            seen.(v) <- true;
            sht := v :: !sht
          end)
        tour;
      let sht = Array.of_list (List.rev !sht) in
      let length = ref 0.0 in
      for i = 0 to n - 1 do
        length := !length +. w.(sht.(i)).(sht.((i + 1) mod n))
      done;
      let parts =
        Array.to_list (Array.map string_of_int sht) |> String.concat " "
      in
      Printf.printf "%s\n%.6f" parts !length
