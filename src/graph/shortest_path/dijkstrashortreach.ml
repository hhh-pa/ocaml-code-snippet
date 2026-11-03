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
    let t = Scanf.scanf " %d" (fun x -> x) in
    let cmp (d1, _) (d2, _) = compare (d1 : int) d2 in
    for _ = 0 to t - 1 do
      let n = Scanf.scanf " %d" (fun x -> x) in
      let m = Scanf.scanf " %d" (fun x -> x) in
      let adj = Array.make (n + 1) [] in
      for _ = 0 to m - 1 do
        let u = Scanf.scanf " %d" (fun x -> x) in
        let v = Scanf.scanf " %d" (fun x -> x) in
        let w = Scanf.scanf " %d" (fun x -> x) in
        adj.(u) <- (v, w) :: adj.(u);
        adj.(v) <- (u, w) :: adj.(v)
      done;
      let s = Scanf.scanf " %d" (fun x -> x) in
      let dist = Array.make (n + 1) (-1) in
      dist.(s) <- 0;
      let pq = MinHeap.create cmp (0, 0) in
      MinHeap.push pq (0, s);
      let rec dijkstra () =
        match MinHeap.pop pq with
        | None -> ()
        | Some (d, u) ->
            if d <= dist.(u) then begin
              List.iter (fun (v, w) ->
                let nd = d + w in
                if dist.(v) = -1 || nd < dist.(v) then begin
                  dist.(v) <- nd;
                  MinHeap.push pq (nd, v)
                end
              ) adj.(u)
            end;
            dijkstra ()
      in
      dijkstra ();
      let first = ref true in
      for i = 1 to n do
        if i <> s then begin
          if not !first then Printf.printf " ";
          Printf.printf "%d" dist.(i);
          first := false
        end
      done;
      Printf.printf "\n"
    done
  with End_of_file -> ()
