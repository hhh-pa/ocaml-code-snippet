let inf = 4_000_000_000_000_000_000L

module MinHeap = struct
  type 'a t = {
    mutable data : 'a array;
    mutable size : int;
    cmp : 'a -> 'a -> int;
  }

  let create ?(capacity = 16) cmp dummy =
    { data = Array.make capacity dummy; size = 0; cmp }

  let push h x =
    if h.size = Array.length h.data then begin
      let new_data = Array.make (2 * Array.length h.data) x in
      Array.blit h.data 0 new_data 0 h.size;
      h.data <- new_data
    end;
    let rec sift_up i =
      if i > 0 then
        let p = (i - 1) / 2 in
        if h.cmp h.data.(i) h.data.(p) < 0 then begin
          let tmp = h.data.(i) in
          h.data.(i) <- h.data.(p);
          h.data.(p) <- tmp;
          sift_up p
        end
    in
    h.data.(h.size) <- x;
    h.size <- h.size + 1;
    sift_up (h.size - 1)

  let pop h =
    if h.size = 0 then None
    else begin
      let res = h.data.(0) in
      h.size <- h.size - 1;
      if h.size > 0 then begin
        h.data.(0) <- h.data.(h.size);
        let rec sift_down i =
          let left = 2 * i + 1 in
          let right = 2 * i + 2 in
          let smallest = ref i in
          if left < h.size && h.cmp h.data.(left) h.data.(!smallest) < 0 then
            smallest := left;
          if right < h.size && h.cmp h.data.(right) h.data.(!smallest) < 0 then
            smallest := right;
          if !smallest <> i then begin
            let tmp = h.data.(i) in
            h.data.(i) <- h.data.(!smallest);
            h.data.(!smallest) <- tmp;
            sift_down !smallest
          end
        in
        sift_down 0
      end;
      Some res
    end
end

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let m = Scanf.scanf " %d" (fun x -> x) in
    let s = Scanf.scanf " %d" (fun x -> x) in
    let t = Scanf.scanf " %d" (fun x -> x) in

    let res = Array.make_matrix n n 0L in
    let cost = Array.make_matrix n n 0L in
    for _ = 0 to m - 1 do
      let u = Scanf.scanf " %d" (fun x -> x) in
      let v = Scanf.scanf " %d" (fun x -> x) in
      let c = Scanf.scanf " %Ld" (fun x -> x) in
      let w = Scanf.scanf " %Ld" (fun x -> x) in
      res.(u).(v) <- Int64.add res.(u).(v) c;
      cost.(u).(v) <- w;
      cost.(v).(u) <- Int64.neg w
    done;

    let pi = Array.make n 0L in
    for _ = 0 to n - 1 do
      for u = 0 to n - 1 do
        for v = 0 to n - 1 do
          if res.(u).(v) > 0L then begin
            let cand = Int64.add pi.(u) cost.(u).(v) in
            if cand < pi.(v) then pi.(v) <- cand
          end
        done
      done
    done;

    let flow = ref 0L in
    let tot = ref 0L in
    let cmp (d1, _) (d2, _) = compare (d1 : int64) d2 in
    let continue_loop = ref true in

    while !continue_loop do
      let dist = Array.make n inf in
      let par = Array.make n (-1) in
      dist.(s) <- 0L;
      let pq = MinHeap.create cmp (0L, 0) in
      MinHeap.push pq (0L, s);
      let rec dijkstra () =
        match MinHeap.pop pq with
        | None -> ()
        | Some (du, u) ->
            if du = dist.(u) then begin
              for v = 0 to n - 1 do
                if res.(u).(v) > 0L then begin
                  let rc = Int64.sub (Int64.add cost.(u).(v) pi.(u)) pi.(v) in
                  let cand = Int64.add dist.(u) rc in
                  if cand < dist.(v) then begin
                    dist.(v) <- cand;
                    par.(v) <- u;
                    MinHeap.push pq (cand, v)
                  end
                end
              done
            end;
            dijkstra ()
      in
      dijkstra ();

      if par.(t) = -1 then
        continue_loop := false
      else begin
        for v = 0 to n - 1 do
          if dist.(v) < inf then
            pi.(v) <- Int64.add pi.(v) dist.(v)
        done;

        let b = ref inf in
        let v = ref t in
        while !v <> s do
          let p = par.(!v) in
          if res.(p).(!v) < !b then b := res.(p).(!v);
          v := p
        done;
        let bottleneck = !b in
        v := t;
        while !v <> s do
          let p = par.(!v) in
          res.(p).(!v) <- Int64.sub res.(p).(!v) bottleneck;
          res.(!v).(p) <- Int64.add res.(!v).(p) bottleneck;
          tot := Int64.add !tot (Int64.mul bottleneck cost.(p).(!v));
          v := p
        done;
        flow := Int64.add !flow bottleneck
      end
    done;
    Printf.printf "%Ld %Ld\n" !flow !tot
  with End_of_file -> ()
