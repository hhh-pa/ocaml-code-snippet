module MaxHeap = struct
  type t = { mutable data: int array; mutable size: int }
  let create cap = { data = Array.make (max 16 cap) 0; size = 0 }
  let push h x =
    if h.size = Array.length h.data then begin
      let new_data = Array.make (h.size * 2) 0 in
      Array.blit h.data 0 new_data 0 h.size;
      h.data <- new_data
    end;
    let rec sift_up i =
      if i > 0 then
        let p = (i - 1) / 2 in
        if h.data.(i) > h.data.(p) then begin
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
    let ret = h.data.(0) in
    h.size <- h.size - 1;
    if h.size > 0 then begin
      h.data.(0) <- h.data.(h.size);
      let rec sift_down i =
        let l = 2 * i + 1 in
        let r = 2 * i + 2 in
        let largest = ref i in
        if l < h.size && h.data.(l) > h.data.(!largest) then largest := l;
        if r < h.size && h.data.(r) > h.data.(!largest) then largest := r;
        if !largest <> i then begin
          let tmp = h.data.(i) in
          h.data.(i) <- h.data.(!largest);
          h.data.(!largest) <- tmp;
          sift_down !largest
        end
      in
      sift_down 0
    end;
    ret

  let is_empty h = h.size = 0
end

let () =
  try
    let v = Scanf.scanf " %d" (fun x -> x) in
    let e = Scanf.scanf " %d" (fun x -> x) in
    let graph = Array.make v [] in
    let indegree = Array.make v 0 in
    for _ = 1 to e do
      let frm = Scanf.scanf " %d" (fun x -> x) in
      let to_node = Scanf.scanf " %d" (fun x -> x) in
      graph.(frm) <- to_node :: graph.(frm);
      indegree.(to_node) <- indegree.(to_node) + 1
    done;

    let heap = MaxHeap.create v in
    for i = v - 1 downto 0 do
      if indegree.(i) = 0 then
        MaxHeap.push heap i
    done;

    let ans = ref [] in
    while not (MaxHeap.is_empty heap) do
      let node = MaxHeap.pop heap in
      ans := node :: !ans;
      List.iter (fun w ->
        indegree.(w) <- indegree.(w) - 1;
        if indegree.(w) = 0 then
          MaxHeap.push heap w
      ) graph.(node)
    done;

    let rev_ans = List.rev !ans in
    let first = ref true in
    List.iter (fun x ->
      if not !first then print_string " ";
      print_int x;
      first := false
    ) rev_ans;
    print_newline ()
  with End_of_file -> ()
