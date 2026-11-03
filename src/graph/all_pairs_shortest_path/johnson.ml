let inf = 1_000_000_000_000_000_000L

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
    let v_count = Scanf.scanf " %d" (fun x -> x) in
    let e_count = Scanf.scanf " %d" (fun x -> x) in
    let edges = Array.init e_count (fun _ ->
      let u = Scanf.scanf " %d" (fun x -> x) in
      let v = Scanf.scanf " %d" (fun x -> x) in
      let w = Scanf.scanf " %Ld" (fun x -> x) in
      (u, v, w)
    ) in
    let adj = Array.make v_count [] in
    Array.iter (fun (u, v, w) -> adj.(u) <- (v, w) :: adj.(u)) edges;

    let h = Array.make v_count 0L in
    let has_neg_cycle = ref false in
    let rec bellman_ford iter =
      if iter < v_count then begin
        let updated = ref false in
        Array.iter (fun (u, v, w) ->
          let nw = Int64.add h.(u) w in
          if nw < h.(v) then begin
            h.(v) <- nw;
            updated := true
          end
        ) edges;
        if !updated then begin
          if iter = v_count - 1 then
            has_neg_cycle := true
          else
            bellman_ford (iter + 1)
        end
      end
    in
    bellman_ford 0;

    if !has_neg_cycle then
      Printf.printf "NEGATIVE CYCLE\n"
    else begin
      let cmp (d1, _) (d2, _) = compare (d1 : int64) d2 in
      for src = 0 to v_count - 1 do
        let d = Array.make v_count inf in
        d.(src) <- 0L;
        let pq = MinHeap.create cmp (0L, 0) in
        MinHeap.push pq (0L, src);
        let rec dijkstra () =
          match MinHeap.pop pq with
          | None -> ()
          | Some (cur_d, u) ->
              if cur_d <= d.(u) then begin
                List.iter (fun (v, w) ->
                  let rw = Int64.sub (Int64.add w h.(u)) h.(v) in
                  let nd = Int64.add cur_d rw in
                  if nd < d.(v) then begin
                    d.(v) <- nd;
                    MinHeap.push pq (nd, v)
                  end
                ) adj.(u)
              end;
              dijkstra ()
        in
        dijkstra ();
        for v = 0 to v_count - 1 do
          if v > 0 then Printf.printf " ";
          if d.(v) >= Int64.div inf 2L then
            Printf.printf "INF"
          else begin
            let real_dist = Int64.sub (Int64.add d.(v) h.(v)) h.(src) in
            Printf.printf "%Ld" real_dist
          end
        done;
        Printf.printf "\n"
      done
    end
  with End_of_file -> ()
