(* One- and multi-dimensional discrete wavelet transform (pyramid
   algorithm) with Daubechies filters: Daub4, Daubs (DAUB4/12/20),
   and Daub4i (DAUB4 on the interval with preconditioning).
   Reference: Press et al., Numerical Recipes 3e, Section 13.11
   (wt1/wtn/Daub4/Daubs/Daub4i from wavelet.h).
   Documented decisions: n must be a power of 2 (the book's stated
   precondition; its bitmask wraparound needs it); n = 1, 2 pass
   through unchanged with flag 0 (the book's silent `n < 4`
   return); Daub4i's filt likewise no-ops below n = 8; unknown
   filter names flag (window-name precedent); wtn flags unless
   every dim >= 1 and the product is a power of 2 (equivalent to
   the book's product-only throw for positive dims); isign >= 0
   transforms, isign < 0 inverts, exactly as the book; Daub4's
   C1/C3 last digits differ from Daubs::c4's and are each kept
   verbatim (0.8365163037378077 vs ...079, -0.1294095225512603
   vs ...604).
   No online-judge problem found -- fixtures are self-generated
   (roundtrip/energy/hand-check oracle), no header link. *)

let fmt6 v =
  let w = if abs_float v < 5e-7 then 0.0 else v in
  Printf.sprintf "%.6f" (w +. 0.0)

let canon v = if abs_float v < 5e-7 then 0.0 else v

(* NOTE: float multiplies feeding +. or -. are let-bound first:
   ocamlopt fuses a direct mul-add/sub nest into FMA, which rounds
   once instead of twice and breaks bit-agreement with the C++
   reference (verified: zero FMA in the object code). *)

let ispow2 n = n >= 1 && (n land (n - 1)) = 0

let d4_c0 = 0.4829629131445341
let d4_c1 = 0.8365163037378077
let d4_c2 = 0.2241438680420134
let d4_c3 = -0.1294095225512603

let c4 =
  [| 0.4829629131445341; 0.8365163037378079; 0.2241438680420134;
     -0.1294095225512604 |]

let c12 =
  [| 0.111540743350; 0.494623890398; 0.751133908021; 0.315250351709;
     -0.226264693965; -0.129766867567; 0.097501605587; 0.027522865530;
     -0.031582039318; 0.000553842201; 0.004777257511; -0.001077301085 |]

let c20 =
  [| 0.026670057901; 0.188176800078; 0.527201188932; 0.688459039454;
     0.281172343661; -0.249846424327; -0.195946274377; 0.127369340336;
     0.093057364604; -0.071394147166; -0.029457536822; 0.033212674059;
     0.003606553567; -0.010733175483; 0.001395351747; 0.001992405295;
     -0.000685856695; -0.000116466855; 0.000093588670; -0.000013264203 |]

type filt = FDaub4 | FDaubs of int | FDaub4i

type daubs_tab = {
  ncof : int;
  ioff : int;
  joff : int;
  cc : float array;
  cr : float array;
}

let daub4_filt a n isign =
  if n >= 4 then (
    let wksp = Array.make n 0.0 in
    let nh = n lsr 1 in
    if isign >= 0 then (
      let i = ref 0 in
      let j = ref 0 in
      while !j < n - 3 do
        let t0 = d4_c0 *. a.(!j) in
        let t1 = d4_c1 *. a.(!j + 1) in
        let t2 = d4_c2 *. a.(!j + 2) in
        let t3 = d4_c3 *. a.(!j + 3) in
        wksp.(!i) <- t0 +. t1 +. t2 +. t3;
        let u0 = d4_c3 *. a.(!j) in
        let u1 = d4_c2 *. a.(!j + 1) in
        let u2 = d4_c1 *. a.(!j + 2) in
        let u3 = d4_c0 *. a.(!j + 3) in
        wksp.(!i + nh) <- u0 -. u1 +. u2 -. u3;
        i := !i + 1;
        j := !j + 2
      done;
      let t0 = d4_c0 *. a.(n - 2) in
      let t1 = d4_c1 *. a.(n - 1) in
      let t2 = d4_c2 *. a.(0) in
      let t3 = d4_c3 *. a.(1) in
      wksp.(!i) <- t0 +. t1 +. t2 +. t3;
      let u0 = d4_c3 *. a.(n - 2) in
      let u1 = d4_c2 *. a.(n - 1) in
      let u2 = d4_c1 *. a.(0) in
      let u3 = d4_c0 *. a.(1) in
      wksp.(!i + nh) <- u0 -. u1 +. u2 -. u3)
    else (
      let t0 = d4_c2 *. a.(nh - 1) in
      let t1 = d4_c1 *. a.(n - 1) in
      let t2 = d4_c0 *. a.(0) in
      let t3 = d4_c3 *. a.(nh) in
      wksp.(0) <- t0 +. t1 +. t2 +. t3;
      let u0 = d4_c3 *. a.(nh - 1) in
      let u1 = d4_c0 *. a.(n - 1) in
      let u2 = d4_c1 *. a.(0) in
      let u3 = d4_c2 *. a.(nh) in
      wksp.(1) <- u0 -. u1 +. u2 -. u3;
      let i = ref 0 in
      let j = ref 2 in
      while !i < nh - 1 do
        let t0 = d4_c2 *. a.(!i) in
        let t1 = d4_c1 *. a.(!i + nh) in
        let t2 = d4_c0 *. a.(!i + 1) in
        let t3 = d4_c3 *. a.(!i + nh + 1) in
        wksp.(!j) <- t0 +. t1 +. t2 +. t3;
        j := !j + 1;
        let u0 = d4_c3 *. a.(!i) in
        let u1 = d4_c0 *. a.(!i + nh) in
        let u2 = d4_c1 *. a.(!i + 1) in
        let u3 = d4_c2 *. a.(!i + nh + 1) in
        wksp.(!j) <- u0 -. u1 +. u2 -. u3;
        j := !j + 1;
        i := !i + 1
      done);
    Array.blit wksp 0 a 0 n)

let daubs_make n =
  let src =
    if n = 4 then Some c4 else if n = 12 then Some c12 else if n = 20 then Some c20 else None
  in
  match src with
  | None -> None
  | Some s ->
    let cc = Array.copy s in
    let cr = Array.make n 0.0 in
    let sgn = ref (-1.0) in
    for i = 0 to n - 1 do
      cr.(n - 1 - i) <- !sgn *. cc.(i);
      sgn := -.(!sgn)
    done;
    Some { ncof = n; ioff = -(n lsr 1); joff = -(n lsr 1); cc; cr }

let daubs_filt a n isign tab =
  if n >= 4 then (
    let wksp = Array.make n 0.0 in
    let nmod = tab.ncof * n in
    let n1 = n - 1 in
    let nh = n lsr 1 in
    if isign >= 0 then (
      let ii = ref 0 in
      let i = ref 0 in
      while !i < n do
        let ni = !i + 1 + nmod + tab.ioff in
        let nj = !i + 1 + nmod + tab.joff in
        for k = 0 to tab.ncof - 1 do
          let jf = n1 land (ni + k + 1) in
          let jr = n1 land (nj + k + 1) in
          let p = tab.cc.(k) *. a.(jf) in
          wksp.(!ii) <- wksp.(!ii) +. p;
          let q = tab.cr.(k) *. a.(jr) in
          wksp.(!ii + nh) <- wksp.(!ii + nh) +. q
        done;
        ii := !ii + 1;
        i := !i + 2
      done)
    else (
      let ii = ref 0 in
      let i = ref 0 in
      while !i < n do
        let ai = a.(!ii) in
        let ai1 = a.(!ii + nh) in
        let ni = !i + 1 + nmod + tab.ioff in
        let nj = !i + 1 + nmod + tab.joff in
        for k = 0 to tab.ncof - 1 do
          let jf = n1 land (ni + k + 1) in
          let jr = n1 land (nj + k + 1) in
          let p = tab.cc.(k) *. ai in
          wksp.(jf) <- wksp.(jf) +. p;
          let q = tab.cr.(k) *. ai1 in
          wksp.(jr) <- wksp.(jr) +. q
        done;
        ii := !ii + 1;
        i := !i + 2
      done);
    Array.blit wksp 0 a 0 n)

let r00 = 0.603332511928053
let r01 = 0.690895531839104
let r02 = -0.398312997698228
let r10 = -0.796543516912183
let r11 = 0.546392713959015
let r12 = -0.258792248333818
let r20 = 0.0375174604524466
let r21 = 0.457327659851769
let r22 = 0.850088102549165
let r23 = 0.223820356983114
let r24 = -0.129222743354319
let r30 = 0.0100372245644139
let r31 = 0.122351043116799
let r32 = 0.227428111655837
let r33 = -0.836602921223654
let r34 = 0.483012921773304
let r43 = 0.443149049637559
let r44 = 0.767556669298114
let r45 = 0.374955331645687
let r46 = 0.190151418429955
let r47 = -0.194233407427412
let r53 = 0.231557595006790
let r54 = 0.401069519430217
let r55 = -0.717579999353722
let r56 = -0.363906959570891
let r57 = 0.371718966535296
let r65 = 0.230389043796969
let r66 = 0.434896997965703
let r67 = 0.870508753349866
let r75 = -0.539822500731772
let r76 = 0.801422961990337
let r77 = -0.257512919478482

let daub4i_filt a n isign =
  if n >= 8 then (
    let wksp = Array.make n 0.0 in
    let nh = n lsr 1 in
    if isign >= 0 then (
      let t0 = r00 *. a.(0) in
      let t1 = r01 *. a.(1) in
      let t2 = r02 *. a.(2) in
      wksp.(0) <- t0 +. t1 +. t2;
      let t0 = r10 *. a.(0) in
      let t1 = r11 *. a.(1) in
      let t2 = r12 *. a.(2) in
      wksp.(nh) <- t0 +. t1 +. t2;
      let t0 = r20 *. a.(0) in
      let t1 = r21 *. a.(1) in
      let t2 = r22 *. a.(2) in
      let t3 = r23 *. a.(3) in
      let t4 = r24 *. a.(4) in
      wksp.(1) <- t0 +. t1 +. t2 +. t3 +. t4;
      let t0 = r30 *. a.(0) in
      let t1 = r31 *. a.(1) in
      let t2 = r32 *. a.(2) in
      let t3 = r33 *. a.(3) in
      let t4 = r34 *. a.(4) in
      wksp.(nh + 1) <- t0 +. t1 +. t2 +. t3 +. t4;
      let i = ref 2 in
      let j = ref 3 in
      while !j < n - 4 do
        let t0 = d4_c0 *. a.(!j) in
        let t1 = d4_c1 *. a.(!j + 1) in
        let t2 = d4_c2 *. a.(!j + 2) in
        let t3 = d4_c3 *. a.(!j + 3) in
        wksp.(!i) <- t0 +. t1 +. t2 +. t3;
        let u0 = d4_c3 *. a.(!j) in
        let u1 = d4_c2 *. a.(!j + 1) in
        let u2 = d4_c1 *. a.(!j + 2) in
        let u3 = d4_c0 *. a.(!j + 3) in
        wksp.(!i + nh) <- u0 -. u1 +. u2 -. u3;
        i := !i + 1;
        j := !j + 2
      done;
      let t0 = r43 *. a.(n - 5) in
      let t1 = r44 *. a.(n - 4) in
      let t2 = r45 *. a.(n - 3) in
      let t3 = r46 *. a.(n - 2) in
      let t4 = r47 *. a.(n - 1) in
      wksp.(nh - 2) <- t0 +. t1 +. t2 +. t3 +. t4;
      let t0 = r53 *. a.(n - 5) in
      let t1 = r54 *. a.(n - 4) in
      let t2 = r55 *. a.(n - 3) in
      let t3 = r56 *. a.(n - 2) in
      let t4 = r57 *. a.(n - 1) in
      wksp.(n - 2) <- t0 +. t1 +. t2 +. t3 +. t4;
      let t0 = r65 *. a.(n - 3) in
      let t1 = r66 *. a.(n - 2) in
      let t2 = r67 *. a.(n - 1) in
      wksp.(nh - 1) <- t0 +. t1 +. t2;
      let t0 = r75 *. a.(n - 3) in
      let t1 = r76 *. a.(n - 2) in
      let t2 = r77 *. a.(n - 1) in
      wksp.(n - 1) <- t0 +. t1 +. t2)
    else (
      let t0 = r00 *. a.(0) in
      let t1 = r10 *. a.(nh) in
      let t2 = r20 *. a.(1) in
      let t3 = r30 *. a.(nh + 1) in
      wksp.(0) <- t0 +. t1 +. t2 +. t3;
      let t0 = r01 *. a.(0) in
      let t1 = r11 *. a.(nh) in
      let t2 = r21 *. a.(1) in
      let t3 = r31 *. a.(nh + 1) in
      wksp.(1) <- t0 +. t1 +. t2 +. t3;
      let t0 = r02 *. a.(0) in
      let t1 = r12 *. a.(nh) in
      let t2 = r22 *. a.(1) in
      let t3 = r32 *. a.(nh + 1) in
      wksp.(2) <- t0 +. t1 +. t2 +. t3;
      (if n = 8 then (
         let t0 = r23 *. a.(1) in
         let t1 = r33 *. a.(5) in
         let t2 = r43 *. a.(2) in
         let t3 = r53 *. a.(6) in
         wksp.(3) <- t0 +. t1 +. t2 +. t3;
         let t0 = r24 *. a.(1) in
         let t1 = r34 *. a.(5) in
         let t2 = r44 *. a.(2) in
         let t3 = r54 *. a.(6) in
         wksp.(4) <- t0 +. t1 +. t2 +. t3)
       else (
         let t0 = r23 *. a.(1) in
         let t1 = r33 *. a.(nh + 1) in
         let t2 = d4_c0 *. a.(2) in
         let t3 = d4_c3 *. a.(nh + 2) in
         wksp.(3) <- t0 +. t1 +. t2 +. t3;
         let t0 = r24 *. a.(1) in
         let t1 = r34 *. a.(nh + 1) in
         let t2 = d4_c1 *. a.(2) in
         let t3 = d4_c2 *. a.(nh + 2) in
         wksp.(4) <- t0 +. t1 +. t2 -. t3;
         let t0 = d4_c2 *. a.(nh - 3) in
         let t1 = d4_c1 *. a.(n - 3) in
         let t2 = r43 *. a.(nh - 2) in
         let t3 = r53 *. a.(n - 2) in
         wksp.(n - 5) <- t0 +. t1 +. t2 +. t3;
         let t0 = d4_c3 *. a.(nh - 3) in
         let t1 = d4_c0 *. a.(n - 3) in
         let t2 = r44 *. a.(nh - 2) in
         let t3 = r54 *. a.(n - 2) in
         wksp.(n - 4) <- t0 -. t1 +. t2 +. t3));
      let i = ref 2 in
      let j = ref 5 in
      while !i < nh - 3 do
        let t0 = d4_c2 *. a.(!i) in
        let t1 = d4_c1 *. a.(!i + nh) in
        let t2 = d4_c0 *. a.(!i + 1) in
        let t3 = d4_c3 *. a.(!i + nh + 1) in
        wksp.(!j) <- t0 +. t1 +. t2 +. t3;
        j := !j + 1;
        let u0 = d4_c3 *. a.(!i) in
        let u1 = d4_c0 *. a.(!i + nh) in
        let u2 = d4_c1 *. a.(!i + 1) in
        let u3 = d4_c2 *. a.(!i + nh + 1) in
        wksp.(!j) <- u0 -. u1 +. u2 -. u3;
        j := !j + 1;
        i := !i + 1
      done;
      let t0 = r45 *. a.(nh - 2) in
      let t1 = r55 *. a.(n - 2) in
      let t2 = r65 *. a.(nh - 1) in
      let t3 = r75 *. a.(n - 1) in
      wksp.(n - 3) <- t0 +. t1 +. t2 +. t3;
      let t0 = r46 *. a.(nh - 2) in
      let t1 = r56 *. a.(n - 2) in
      let t2 = r66 *. a.(nh - 1) in
      let t3 = r76 *. a.(n - 1) in
      wksp.(n - 2) <- t0 +. t1 +. t2 +. t3;
      let t0 = r47 *. a.(nh - 2) in
      let t1 = r57 *. a.(n - 2) in
      let t2 = r67 *. a.(nh - 1) in
      let t3 = r77 *. a.(n - 1) in
      wksp.(n - 1) <- t0 +. t1 +. t2 +. t3);
    Array.blit wksp 0 a 0 n)

let daub4i_condition a n isign =
  if n >= 4 then (
    if isign >= 0 then (
      let t0 = 0.324894048898962 *. a.(0) in
      let t1 = 0.0371580151158803 *. a.(1) in
      let s0 = 1.00144540498130 *. a.(1) in
      let s1 = 1.08984305289504 *. a.(n - 2) in
      let u0 = -0.800813234246437 *. a.(n - 2) in
      let u1 = 2.09629288435324 *. a.(n - 1) in
      a.(0) <- t0 +. t1;
      a.(1) <- s0;
      a.(n - 2) <- s1;
      a.(n - 1) <- u0 +. u1)
    else (
      let t0 = 3.07792649138669 *. a.(0) in
      let t1 = 0.114204567242137 *. a.(1) in
      let s0 = 0.998556681198888 *. a.(1) in
      let s1 = 0.917563310922261 *. a.(n - 2) in
      let u0 = 0.350522032550918 *. a.(n - 2) in
      let u1 = 0.477032578540915 *. a.(n - 1) in
      a.(0) <- t0 -. t1;
      a.(1) <- s0;
      a.(n - 2) <- s1;
      a.(n - 1) <- u0 +. u1))

let apply_filt a n isign f tab =
  match f with
  | FDaub4 -> daub4_filt a n isign
  | FDaub4i -> daub4i_filt a n isign
  | FDaubs _ -> (
    match tab with
    | Some t -> daubs_filt a n isign t
    | None -> ())

let apply_condition a n isign f =
  match f with
  | FDaub4i -> daub4i_condition a n isign
  | _ -> ()

let wt1 a isign f tab =
  let n = Array.length a in
  if n >= 4 then (
    if isign >= 0 then (
      apply_condition a n 1 f;
      let nn = ref n in
      while !nn >= 4 do
        apply_filt a !nn isign f tab;
        nn := !nn lsr 1
      done)
    else (
      let nn = ref 4 in
      while !nn <= n do
        apply_filt a !nn isign f tab;
        nn := !nn lsl 1
      done;
      apply_condition a n (-1) f))

let wtn a nn isign f tab =
  let ntot = Array.fold_left ( * ) 1 nn in
  if ispow2 ntot then (
    let nprev = ref 1 in
    Array.iter
      (fun n ->
        let wksp = Array.make n 0.0 in
        let nnew = n * !nprev in
        (if n > 4 then (
           let i2 = ref 0 in
           while !i2 < ntot do
             for i1 = 0 to !nprev - 1 do
               let i3 = ref (i1 + !i2) in
               for k = 0 to n - 1 do
                 wksp.(k) <- a.(!i3);
                 i3 := !i3 + !nprev
               done;
               if isign >= 0 then (
                 apply_condition wksp n 1 f;
                 let nt = ref n in
                 while !nt >= 4 do
                   apply_filt wksp !nt isign f tab;
                   nt := !nt lsr 1
                 done)
               else (
                 let nt = ref 4 in
                 while !nt <= n do
                   apply_filt wksp !nt isign f tab;
                   nt := !nt lsl 1
                 done;
                 apply_condition wksp n (-1) f);
               i3 := i1 + !i2;
               for k = 0 to n - 1 do
                 a.(!i3) <- wksp.(k);
                 i3 := !i3 + !nprev
               done
             done;
             i2 := !i2 + nnew
           done));
        nprev := nnew)
      nn;
    true)
  else false

let parse_filt s =
  if s = "DAUB4" then Some FDaub4
  else if s = "DAUBS4" then Some (FDaubs 4)
  else if s = "DAUBS12" then Some (FDaubs 12)
  else if s = "DAUBS20" then Some (FDaubs 20)
  else if s = "DAUB4I" then Some FDaub4i
  else None

let make_tab f =
  match f with
  | FDaubs n -> daubs_make n
  | _ -> None

let emit a flag =
  let parts = Array.to_list (Array.map (fun v -> fmt6 (canon v)) a) in
  print_endline (String.concat " " (parts @ [ string_of_int flag ]))

let emit_zeros n flag =
  let parts = List.init n (fun _ -> "0.000000") in
  print_endline (String.concat " " (parts @ [ string_of_int flag ]))

let () =
  let toks =
    String.split_on_char ' '
      (String.concat " "
         (let rec rd acc =
            try rd (input_line stdin :: acc) with End_of_file -> List.rev acc
          in
          rd []))
  in
  let toks = Array.of_list (List.filter (fun s -> s <> "") toks) in
  if Array.length toks = 0 then exit 0;
  let op = toks.(0) in
  if op = "WT1" then (
    let n = int_of_string toks.(1) in
    let f = parse_filt toks.(2) in
    let isign = int_of_string toks.(3) in
    let shapeok = n >= 1 && n <= 1048576 in
    let ok =
      match f with
      | None -> false
      | Some _ -> shapeok && ispow2 n
    in
    if ok then (
      let a = Array.init n (fun i -> float_of_string toks.(4 + i)) in
      let ff =
        match f with
        | Some x -> x
        | None -> FDaub4
      in
      wt1 a isign ff (make_tab ff);
      emit a 0)
    else emit_zeros (if shapeok then n else 0) 1)
  else if op = "WTN" then (
    let ndim = int_of_string toks.(1) in
    let nd = max ndim 0 in
    let dims = Array.init nd (fun i -> int_of_string toks.(2 + i)) in
    let f = parse_filt toks.(2 + nd) in
    let isign = int_of_string toks.(3 + nd) in
    let dimsok = ref (ndim >= 1) in
    let prod = ref 1 in
    Array.iter
      (fun d ->
        if !dimsok then (
          if d < 1 || !prod > 1048576 / d then dimsok := false
          else prod := !prod * d))
      dims;
    let ok =
      match f with
      | None -> false
      | Some _ -> !dimsok && ispow2 !prod
    in
    if ok then (
      let a = Array.init !prod (fun i -> float_of_string toks.(4 + nd + i)) in
      let ff =
        match f with
        | Some x -> x
        | None -> FDaub4
      in
      if wtn a dims isign ff (make_tab ff) then emit a 0
      else emit_zeros !prod 1)
    else emit_zeros (if !dimsok then !prod else 0) 1)
