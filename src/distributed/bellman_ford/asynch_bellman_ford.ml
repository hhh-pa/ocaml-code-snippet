type edge = { u: int; v: int; w: int64 }

let inf = 4000000000000000000L

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let m = Scanf.scanf " %d" (fun x -> x) in
    let edges = Array.init m (fun _ ->
      let u = Scanf.scanf " %d" (fun x -> x) in
      let v = Scanf.scanf " %d" (fun x -> x) in
      let w = Scanf.scanf " %Ld" (fun x -> x) in
      { u; v; w }
    ) in
    let src = Scanf.scanf " %d" (fun x -> x) in
    let dist = Array.make n inf in
    dist.(src) <- 0L;
    let rounds = ref 0 in
    let r = ref 0 in
    let ch = ref true in
    while !r < n - 1 && !ch do
      incr rounds;
      incr r;
      ch := false;
      for i = 0 to m - 1 do
        let e = edges.(i) in
        if dist.(e.u) <> inf then
          let nw = Int64.add dist.(e.u) e.w in
          if nw < dist.(e.v) then begin
            dist.(e.v) <- nw;
            ch := true
          end
      done
    done;
    let neg = ref false in
    for i = 0 to m - 1 do
      let e = edges.(i) in
      if dist.(e.u) <> inf && Int64.add dist.(e.u) e.w < dist.(e.v) then
        neg := true
    done;
    if !neg then begin
      Printf.printf "NEGATIVE CYCLE\n";
      Printf.printf "MESSAGES: %Ld\n" (Int64.mul (Int64.of_int m) (Int64.of_int n))
    end else begin
      for i = 0 to n - 1 do
        if dist.(i) = inf then
          Printf.printf "%d INF\n" i
        else
          Printf.printf "%d %Ld\n" i dist.(i)
      done;
      Printf.printf "MESSAGES: %Ld\n" (Int64.mul (Int64.of_int m) (Int64.of_int !rounds))
    end
  with End_of_file -> ()
