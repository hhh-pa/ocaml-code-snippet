let read_all_tokens () =
  let content =
    let b = Buffer.create 4096 in
    let buf = Bytes.create 4096 in
    let rec loop () =
      let n = input stdin buf 0 4096 in
      if n > 0 then (
        Buffer.add_subbytes b buf 0 n;
        loop ()
      )
    in
    loop ();
    Buffer.contents b
  in
  let len = String.length content in
  let tokens = ref [] in
  let i = ref 0 in
  while !i < len do
    while !i < len && (let c = content.[!i] in c = ' ' || c = '\t' || c = '\r' || c = '\n') do
      incr i
    done;
    if !i < len then (
      let start = !i in
      while !i < len && (let c = content.[!i] in c <> ' ' && c <> '\t' && c <> '\r' && c <> '\n') do
        incr i
      done;
      tokens := String.sub content start (!i - start) :: !tokens
    )
  done;
  Array.of_list (List.rev !tokens)

type pt = { x : int64; y : int64 }

let cross a b c =
  Int64.sub
    (Int64.mul (Int64.sub b.x a.x) (Int64.sub c.y a.y))
    (Int64.mul (Int64.sub b.y a.y) (Int64.sub c.x a.x))

let inside p a b c =
  let c1 = cross a b p in
  let c2 = cross b c p in
  let c3 = cross c a p in
  c1 > 0L && c2 > 0L && c3 > 0L

let () =
  let tokens = read_all_tokens () in
  if Array.length tokens >= 1 then (
    let n = int_of_string tokens.(0) in
    let idx = ref 1 in
    let pin = Array.init n (fun _ ->
      let x = Int64.of_string tokens.(!idx) in
      let y = Int64.of_string tokens.(!idx + 1) in
      idx := !idx + 2;
      { x; y }
    ) in

    let area2 = ref 0L in
    for i = 0 to n - 1 do
      let a = pin.(i) in
      let b = pin.((i + 1) mod n) in
      area2 := Int64.add !area2 (Int64.sub (Int64.mul a.x b.y) (Int64.mul a.y b.x))
    done;

    let pts = Array.copy pin in
    let w2o = Array.init n (fun i -> i) in
    if !area2 < 0L then (
      for i = 0 to n / 2 - 1 do
        let tmp = pts.(i) in
        pts.(i) <- pts.(n - 1 - i);
        pts.(n - 1 - i) <- tmp
      done;
      for i = 0 to n - 1 do
        w2o.(i) <- n - 1 - i
      done
    );

    let poly = ref (Array.to_list (Array.init n (fun i -> i))) in
    let tris = ref [] in

    let convex a b c = cross pts.(a) pts.(b) pts.(c) > 0L in

    while List.length !poly > 3 do
      let m = List.length !poly in
      let poly_arr = Array.of_list !poly in
      let found = ref (-1) in
      let k = ref 0 in
      while !k < m && !found = -1 do
        let a = poly_arr.((!k - 1 + m) mod m) in
        let b = poly_arr.(!k) in
        let c = poly_arr.((!k + 1) mod m) in
        if convex a b c then (
          let ok = ref true in
          let v_idx = ref 0 in
          while !v_idx < m && !ok do
            let v = poly_arr.(!v_idx) in
            if v <> a && v <> b && v <> c then (
              if inside pts.(v) pts.(a) pts.(b) pts.(c) then ok := false
            );
            incr v_idx
          done;
          if !ok then found := !k
        );
        incr k
      done;
      let f = !found in
      let a = poly_arr.((f - 1 + m) mod m) in
      let b = poly_arr.(f) in
      let c = poly_arr.((f + 1) mod m) in
      tris := [a; b; c] :: !tris;
      let new_poly = ref [] in
      for i = 0 to m - 1 do
        if i <> f then new_poly := poly_arr.(i) :: !new_poly
      done;
      poly := List.rev !new_poly
    done;

    let poly_arr = Array.of_list !poly in
    tris := [poly_arr.(0); poly_arr.(1); poly_arr.(2)] :: !tris;
    let tris = Array.of_list (List.rev !tris) in
    let num_tris = Array.length tris in

    let emap = Hashtbl.create (num_tris * 3) in
    for t = 0 to num_tris - 1 do
      let tr = tris.(t) in
      let tr_arr = Array.of_list tr in
      for e = 0 to 2 do
        let u = tr_arr.(e) in
        let v = tr_arr.((e + 1) mod 3) in
        let a, b = if u > v then (v, u) else (u, v) in
        let prev = match Hashtbl.find_opt emap (a, b) with Some l -> l | None -> [] in
        Hashtbl.replace emap (a, b) (t :: prev)
      done
    done;

    let color = Array.make n (-1) in
    let tr0 = Array.of_list tris.(0) in
    color.(tr0.(0)) <- 0;
    color.(tr0.(1)) <- 1;
    color.(tr0.(2)) <- 2;

    let vis = Array.make num_tris false in
    vis.(0) <- true;
    let q = Queue.create () in
    Queue.push 0 q;

    while not (Queue.is_empty q) do
      let t = Queue.pop q in
      let tr = Array.of_list tris.(t) in
      for e = 0 to 2 do
        let u = tr.(e) in
        let v = tr.((e + 1) mod 3) in
        let a, b = if u > v then (v, u) else (u, v) in
        match Hashtbl.find_opt emap (a, b) with
        | Some slist ->
          List.iter (fun s ->
            if not vis.(s) then (
              vis.(s) <- true;
              let tr_s = tris.(s) in
              let w = ref (-1) in
              List.iter (fun x ->
                if x <> u && x <> v then w := x
              ) tr_s;
              color.(!w) <- 3 - color.(u) - color.(v);
              Queue.push s q
            )
          ) slist
        | None -> ()
      done
    done;

    let cls = Array.init 3 (fun _ -> ref []) in
    for i = 0 to n - 1 do
      cls.(color.(i)) := i :: !(cls.(color.(i)))
    done;
    for c = 0 to 2 do
      cls.(c) := List.sort compare !(cls.(c))
    done;

    let best = ref 0 in
    for c = 1 to 2 do
      let len_c = List.length !(cls.(c)) in
      let len_b = List.length !(cls.(!best)) in
      if len_c < len_b || (len_c = len_b && !(cls.(c)) < !(cls.(!best))) then
        best := c
    done;

    let guards = List.map (fun w -> w2o.(w)) !(cls.(!best)) in
    let guards = List.sort compare guards in

    Printf.printf "%d\n" (List.length guards);
    let gstrs = List.map string_of_int guards in
    Printf.printf "%s\n" (String.concat " " gstrs)
  )
