type dsu = {
  parent : int array;
  rank : int array;
}

let create_dsu n =
  { parent = Array.init n (fun i -> i);
    rank = Array.make n 0 }

let rec find dsu i =
  if dsu.parent.(i) = i then i
  else begin
    let root = find dsu dsu.parent.(i) in
    dsu.parent.(i) <- root;
    root
  end

let unite dsu i j =
  let root_i = find dsu i in
  let root_j = find dsu j in
  if root_i = root_j then false
  else begin
    let r_i, r_j =
      if dsu.rank.(root_i) < dsu.rank.(root_j) then (root_j, root_i)
      else (root_i, root_j)
    in
    dsu.parent.(r_j) <- r_i;
    if dsu.rank.(r_i) = dsu.rank.(r_j) then
      dsu.rank.(r_i) <- dsu.rank.(r_i) + 1;
    true
  end

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let m = Scanf.scanf " %d" (fun x -> x) in
    let edges = Array.init m (fun _ ->
      let u = Scanf.scanf " %d" (fun x -> x) in
      let v = Scanf.scanf " %d" (fun x -> x) in
      let w = Scanf.scanf " %Ld" (fun x -> x) in
      (u, v, w)
    ) in
    let order = Array.init m (fun i -> i) in
    Array.sort (fun i j ->
      let (u1, v1, w1) = edges.(i) in
      let (u2, v2, w2) = edges.(j) in
      let c = compare (w1 : int64) w2 in
      if c <> 0 then c
      else
        let c2 = compare (u1 : int) u2 in
        if c2 <> 0 then c2
        else compare (v1 : int) v2
    ) order;
    let dsu = create_dsu n in
    let used = Array.make m false in
    let mw = ref 0L in
    let cnt = ref 0 in
    Array.iter (fun i ->
      let (u, v, w) = edges.(i) in
      if unite dsu u v then begin
        used.(i) <- true;
        mw := Int64.add !mw w;
        incr cnt
      end
    ) order;
    if !cnt <> n - 1 then
      Printf.printf "DISCONNECTED\n"
    else begin
      let ta = Array.make n [] in
      for i = 0 to m - 1 do
        if used.(i) then begin
          let (u, v, w) = edges.(i) in
          ta.(u) <- (v, w) :: ta.(u);
          ta.(v) <- (u, w) :: ta.(v)
        end
      done;
      let maxe s t =
        if s = t then 0L
        else begin
          let par = Array.make n (-1) in
          let pw = Array.make n 0L in
          let q = Queue.create () in
          Queue.add s q;
          par.(s) <- s;
          let found = ref false in
          while not (Queue.is_empty q) && not !found do
            let u = Queue.take q in
            if u = t then found := true
            else
              List.iter (fun (w_node, c) ->
                if par.(w_node) = -1 then begin
                  par.(w_node) <- u;
                  pw.(w_node) <- c;
                  Queue.add w_node q
                end
              ) ta.(u)
          done;
          let mx = ref 0L in
          let x = ref t in
          while !x <> s do
            if pw.(!x) > !mx then mx := pw.(!x);
            x := par.(!x)
          done;
          !mx
        end
      in
      let best = ref 0L in
      let any_found = ref false in
      for i = 0 to m - 1 do
        if not used.(i) then begin
          let (u, v, w) = edges.(i) in
          let c = Int64.sub (Int64.add !mw w) (maxe u v) in
          if not !any_found || c < !best then begin
            best := c;
            any_found := true
          end
        end
      done;
      Printf.printf "%Ld\n" !mw;
      if !any_found then
        Printf.printf "%Ld\n" !best
      else
        Printf.printf "NO SECOND\n"
    end
  with End_of_file -> ()
