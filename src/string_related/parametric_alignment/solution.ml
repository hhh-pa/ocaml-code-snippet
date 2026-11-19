(* 1D parametric alignment: exact decomposition over gap penalty. *)

let neg_inf = -4000000000000000000L

let rec igcd a b =
  let a = Int64.abs a and b = Int64.abs b in
  if b = 0L then a else igcd b (Int64.rem a b)

let make_frac n d =
  let n, d = if d < 0L then (Int64.neg n, Int64.neg d) else (n, d) in
  let g = igcd n d in
  (Int64.div n g, Int64.div d g)

let frac_lt (an, ad) (bn, bd) = Int64.mul an bd < Int64.mul bn ad
let frac_le (an, ad) (bn, bd) = Int64.mul an bd <= Int64.mul bn ad

let fmt_frac (n, d) =
  if d = 1L then Printf.sprintf "%Ld" n else Printf.sprintf "%Ld/%Ld" n d

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
  | _ :: ms :: mms :: g0s :: g1s :: s1 :: s2 :: _ ->
      let vmatch = Int64.of_string ms
      and mismatch = Int64.of_string mms
      and g0 = Int64.of_string g0s
      and g1 = Int64.of_string g1s in
      let n = String.length s1
      and m = String.length s2
      and k = String.length s1 + String.length s2 in
      let dp =
        Array.init (n + 1) (fun _ ->
            Array.init (m + 1) (fun _ -> Array.make (k + 1) neg_inf))
      in
      let par =
        Array.init (n + 1) (fun _ ->
            Array.init (m + 1) (fun _ -> Array.make (k + 1) 0))
      in
      dp.(0).(0).(0) <- 0L;
      for i = 1 to n do
        dp.(i).(0).(i) <- 0L;
        par.(i).(0).(i) <- 2
      done;
      for j = 1 to m do
        dp.(0).(j).(j) <- 0L;
        par.(0).(j).(j) <- 3
      done;
      for i = 1 to n do
        for j = 1 to m do
          let sub = if s1.[i - 1] = s2.[j - 1] then vmatch else mismatch in
          for kk = 0 to k do
            let best = ref neg_inf and bp = ref 0 in
            if dp.(i - 1).(j - 1).(kk) <> neg_inf then begin
              best := Int64.add dp.(i - 1).(j - 1).(kk) sub;
              bp := 1
            end;
            if
              kk > 0
              && dp.(i - 1).(j).(kk - 1) <> neg_inf
              && dp.(i - 1).(j).(kk - 1) > !best
            then begin
              best := dp.(i - 1).(j).(kk - 1);
              bp := 2
            end;
            if
              kk > 0
              && dp.(i).(j - 1).(kk - 1) <> neg_inf
              && dp.(i).(j - 1).(kk - 1) > !best
            then begin
              best := dp.(i).(j - 1).(kk - 1);
              bp := 3
            end;
            dp.(i).(j).(kk) <- !best;
            par.(i).(j).(kk) <- !bp
          done
        done
      done;
      let lines = ref [] in
      for s = 0 to k do
        if dp.(n).(m).(s) <> neg_inf then begin
          let mt = ref 0
          and ms = ref 0
          and i = ref n
          and j = ref m
          and kk = ref s in
          while !i > 0 || !j > 0 do
            match par.(!i).(!j).(!kk) with
            | 1 ->
                if s1.[!i - 1] = s2.[!j - 1] then incr mt else incr ms;
                decr i;
                decr j
            | 2 ->
                decr i;
                decr kk
            | _ ->
                decr j;
                decr kk
          done;
          lines :=
            (Int64.neg (Int64.of_int s), dp.(n).(m).(s), !mt, !ms, s) :: !lines
        end
      done;
      let lines =
        List.sort
          (fun (_, _, _, _, s1) (_, _, _, _, s2) -> compare s2 s1)
          !lines
      in
      let uniq =
        List.fold_left
          (fun acc ln ->
            let sl, cc, _, _, _ = ln in
            match acc with
            | (psl, pcc, _, _, _) :: _ when psl = sl && pcc = cc -> acc
            | _ -> ln :: acc)
          [] lines
        |> List.rev
      in
      let cross (sa, ca, _, _, _) (sb, cb, _, _, _) =
        make_frac (Int64.sub cb ca) (Int64.sub sa sb)
      in
      let hull = ref [] in
      List.iter
        (fun ln ->
          let cont = ref true in
          while List.length !hull >= 2 && !cont do
            let h = !hull in
            let l1 = List.nth h (List.length h - 2)
            and l2 = List.nth h (List.length h - 1) in
            if frac_le (cross l2 ln) (cross l1 l2) then
              hull := List.filteri (fun i _ -> i < List.length h - 1) h
            else cont := false
          done;
          hull := !hull @ [ ln ])
        uniq;
      let hull = !hull in
      let g0f = (g0, 1L) and g1f = (g1, 1L) in
      let segs = ref [] in
      List.iteri
        (fun i ln ->
          let _, _, mt, ms, s = ln in
          let lo = if i = 0 then g0f else cross (List.nth hull (i - 1)) ln in
          let hi =
            if i + 1 = List.length hull then g1f
            else cross ln (List.nth hull (i + 1))
          in
          let lo = if frac_lt lo g0f then g0f else lo in
          let hi = if frac_lt g1f hi then g1f else hi in
          if frac_lt lo hi then segs := (lo, hi, mt, ms, s) :: !segs)
        hull;
      let segs =
        if !segs = [] then
          let best =
            List.fold_left
              (fun ((bsl, bc, _, _, _) as acc) ((sl, cc, _, _, _) as ln) ->
                if
                  Int64.add cc (Int64.mul sl g0)
                  > Int64.add bc (Int64.mul bsl g0)
                then ln
                else acc)
              (List.hd hull) (List.tl hull)
          in
          let _, _, mt, ms, s = best in
          [ (g0f, g1f, mt, ms, s) ]
        else List.rev !segs
      in
      print_string
        (String.concat "\n"
           (List.map
              (fun (lo, hi, mt, ms, s) ->
                Printf.sprintf "%s %s %d %d %d" (fmt_frac lo) (fmt_frac hi) mt
                  ms s)
              segs))
  | _ -> ()
