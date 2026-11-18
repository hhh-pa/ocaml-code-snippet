(* Pupils, windows, vignetting (geom ch 5). *)

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

let snap x = if abs_float x < 5e-7 then 0.0 else x

let image_of a b c d z_end rim forward =
  let sp =
    if d <> 0.0 then if forward then -.b /. d else b /. d
    else if b > 0.0 then 1e15
    else if b < 0.0 then -1e15
    else 1e15
  in
  if forward then (z_end +. sp, abs_float (a +. (sp *. c)) *. rim)
  else (z_end -. sp, abs_float (a -. (sp *. c)) *. rim)

let run_chain zs fs order zstart forward =
  let a = ref 1.0 in
  let b = ref 0.0 in
  let c = ref 0.0 in
  let d = ref 1.0 in
  let zcur = ref zstart in
  List.iter
    (fun i ->
      let dz = zs.(i) -. !zcur in
      a := !a +. (!c *. dz);
      b := !b +. (!d *. dz);
      if fs.(i) <> 0.0 then
        if forward then begin
          c := !c -. (!a /. fs.(i));
          d := !d -. (!b /. fs.(i))
        end
        else begin
          c := !c +. (!a /. fs.(i));
          d := !d +. (!b /. fs.(i))
        end;
      zcur := zs.(i))
    order;
  (!a, !b, !c, !d, !zcur)

let vignette zs fs aps ho zo s =
  let n = Array.length zs in
  let uc = ref ((0.0 -. ho) /. (zs.(s) -. zo)) in
  let up = ref ((aps.(s) -. ho) /. (zs.(s) -. zo)) in
  let um = ref ((-.aps.(s) -. ho) /. (zs.(s) -. zo)) in
  let hc = ref ho in
  let hp = ref ho in
  let hm = ref ho in
  let zcur = ref zo in
  let vig = ref 1.0 in
  for i = 0 to n - 1 do
    let dz = zs.(i) -. !zcur in
    hc := !hc +. (!uc *. dz);
    hp := !hp +. (!up *. dz);
    hm := !hm +. (!um *. dz);
    let bw = max (abs_float (!hp -. !hc)) (abs_float (!hm -. !hc)) in
    let f = if bw = 0.0 then 1.0 else (aps.(i) -. abs_float !hc) /. bw in
    if f < !vig then vig := f;
    if fs.(i) <> 0.0 then begin
      uc := !uc -. (!hc /. fs.(i));
      up := !up -. (!hp /. fs.(i));
      um := !um -. (!hm /. fs.(i))
    end;
    zcur := zs.(i)
  done;
  max 0.0 (min 1.0 !vig)

let () =
  match split_ws (read_all ()) with
  | [] -> ()
  | toks ->
      let arr = Array.of_list toks in
      let ho = float_of_string arr.(0) in
      let zo = float_of_string arr.(1) in
      let n = int_of_string arr.(2) in
      let zs = Array.init n (fun i -> float_of_string arr.(3 + (3 * i))) in
      let fs = Array.init n (fun i -> float_of_string arr.(4 + (3 * i))) in
      let aps = Array.init n (fun i -> float_of_string arr.(5 + (3 * i))) in
      let s = int_of_string arr.(3 + (3 * n)) in
      let w = int_of_string arr.(4 + (3 * n)) in
      let back = List.init s (fun i -> s - 1 - i) in
      let fwd = List.init (n - s - 1) (fun i -> s + 1 + i) in
      let wback = List.init w (fun i -> w - 1 - i) in
      let a, b, c, d, zcur = run_chain zs fs back zs.(s) false in
      let enz, enr = image_of a b c d zcur aps.(s) false in
      let a, b, c, d, zcur = run_chain zs fs fwd zs.(s) true in
      let exz, exr = image_of a b c d zcur aps.(s) true in
      let a, b, c, d, zcur = run_chain zs fs wback zs.(w) false in
      let wnz, wnr = image_of a b c d zcur aps.(w) false in
      let vig = vignette zs fs aps ho zo s in
      Printf.printf
        "ENPUPIL %.6f %.6f\nEXPUPIL %.6f %.6f\nENWINDOW %.6f %.6f\nVIG %.6f"
        (snap enz) (snap enr) (snap exz) (snap exr) (snap wnz) (snap wnr)
        (snap vig)
