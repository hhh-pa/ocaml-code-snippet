let inf = 4_000_000_000_000_000_000L

let rec solve n root es ans =
  let inw = Array.make n inf in
  let pre = Array.make n (-1) in
  List.iter (fun (u, v, w) ->
    if u <> v && w < inw.(v) then begin
      inw.(v) <- w;
      pre.(v) <- u
    end
  ) es;
  inw.(root) <- 0L;
  let has_unreachable = ref false in
  for i = 0 to n - 1 do
    if inw.(i) = inf then has_unreachable := true
  done;
  if !has_unreachable then
    Printf.printf "NO ARBORESCENCE\n"
  else begin
    let comp = Array.make n (-1) in
    let vis = Array.make n (-1) in
    let cn = ref 0 in
    for i = 0 to n - 1 do
      let v = ref i in
      while vis.(!v) <> i && comp.(!v) = -1 && !v <> root do
        vis.(!v) <- i;
        v := pre.(!v)
      done;
      if !v <> root && comp.(!v) = -1 then begin
        let j = ref pre.(!v) in
        while !j <> !v do
          comp.(!j) <- !cn;
          j := pre.(!j)
        done;
        comp.(!v) <- !cn;
        incr cn
      end
    done;
    let new_ans = ref ans in
    for i = 0 to n - 1 do
      new_ans := Int64.add !new_ans inw.(i)
    done;
    if !cn = 0 then
      Printf.printf "%Ld\n" !new_ans
    else begin
      for i = 0 to n - 1 do
        if comp.(i) = -1 then begin
          comp.(i) <- !cn;
          incr cn
        end
      done;
      let ne = ref [] in
      List.iter (fun (u, v, w) ->
        if comp.(u) <> comp.(v) then
          ne := (comp.(u), comp.(v), Int64.sub w inw.(v)) :: !ne
      ) es;
      solve !cn comp.(root) (List.rev !ne) !new_ans
    end
  end

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let m = Scanf.scanf " %d" (fun x -> x) in
    let root = Scanf.scanf " %d" (fun x -> x) in
    let es = ref [] in
    for _ = 0 to m - 1 do
      let u = Scanf.scanf " %d" (fun x -> x) in
      let v = Scanf.scanf " %d" (fun x -> x) in
      let w = Scanf.scanf " %Ld" (fun x -> x) in
      es := (u, v, w) :: !es
    done;
    solve n root (List.rev !es) 0L
  with End_of_file -> ()
