let inf = 4_000_000_000_000_000_000L

module IntMinHeap = struct
  type t = {
    mutable data : int array;
    mutable size : int;
  }

  let create ?(capacity = 16) () =
    { data = Array.make capacity 0; size = 0 }

  let push h x =
    if h.size = Array.length h.data then begin
      let new_data = Array.make (2 * Array.length h.data) 0 in
      Array.blit h.data 0 new_data 0 h.size;
      h.data <- new_data
    end;
    let rec sift_up i =
      if i > 0 then
        let p = (i - 1) / 2 in
        if h.data.(i) < h.data.(p) then begin
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
          if left < h.size && h.data.(left) < h.data.(!smallest) then
            smallest := left;
          if right < h.size && h.data.(right) < h.data.(!smallest) then
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
    let adj = Array.make n [] in
    let ind = Array.make n 0 in
    for _ = 0 to m - 1 do
      let u = Scanf.scanf " %d" (fun x -> x) in
      let v = Scanf.scanf " %d" (fun x -> x) in
      let w = Scanf.scanf " %Ld" (fun x -> x) in
      adj.(u) <- (v, w) :: adj.(u);
      ind.(v) <- ind.(v) + 1
    done;
    for i = 0 to n - 1 do
      adj.(i) <- List.rev adj.(i)
    done;
    let pq = IntMinHeap.create () in
    for i = 0 to n - 1 do
      if ind.(i) = 0 then IntMinHeap.push pq i
    done;
    let topo = ref [] in
    let rec kahn () =
      match IntMinHeap.pop pq with
      | None -> ()
      | Some u ->
          topo := u :: !topo;
          List.iter (fun (v, _) ->
            ind.(v) <- ind.(v) - 1;
            if ind.(v) = 0 then IntMinHeap.push pq v
          ) adj.(u);
          kahn ()
    in
    kahn ();
    let topo_order = List.rev !topo in
    let ds = Array.make n inf in
    let dl = Array.make n (Int64.neg inf) in
    ds.(s) <- 0L;
    dl.(s) <- 0L;
    let neg_half_inf = Int64.div (Int64.neg inf) 2L in
    List.iter (fun u ->
      List.iter (fun (v, w) ->
        let cand_s = Int64.add ds.(u) w in
        if cand_s < ds.(v) then ds.(v) <- cand_s;
        if dl.(u) > neg_half_inf then begin
          let cand_l = Int64.add dl.(u) w in
          if cand_l > dl.(v) then dl.(v) <- cand_l
        end
      ) adj.(u)
    ) topo_order;
    let half_inf = Int64.div inf 2L in
    for i = 0 to n - 1 do
      if i > 0 then Printf.printf " ";
      if ds.(i) >= half_inf then Printf.printf "INF"
      else Printf.printf "%Ld" ds.(i)
    done;
    Printf.printf "\n";
    for i = 0 to n - 1 do
      if i > 0 then Printf.printf " ";
      if dl.(i) <= neg_half_inf then Printf.printf "-INF"
      else Printf.printf "%Ld" dl.(i)
    done;
    Printf.printf "\n"
  with End_of_file -> ()
