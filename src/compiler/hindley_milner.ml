module IntMap = Map.Make(Int)
module IntSet = Set.Make(Int)
module StringMap = Map.Make(String)

type ty =
  | TVar of int
  | TCon of string
  | TArr of ty * ty

type subst = ty IntMap.t

let rec apply_s (s : subst) (t : ty) : ty =
  match t with
  | TVar id ->
    (match IntMap.find_opt id s with
     | Some t' -> apply_s s t'
     | None -> t)
  | TCon _ -> t
  | TArr (l, r) -> TArr (apply_s s l, apply_s s r)

let rec ftv (t : ty) (acc : IntSet.t) : IntSet.t =
  match t with
  | TVar id -> IntSet.add id acc
  | TCon _ -> acc
  | TArr (l, r) -> ftv r (ftv l acc)

let occurs (v : int) (t : ty) : bool =
  IntSet.mem v (ftv t IntSet.empty)

let comp_s (s1 : subst) (s2 : subst) : subst =
  let r = IntMap.map (fun v -> apply_s s1 v) s2 in
  IntMap.fold (fun k v acc ->
    if IntMap.mem k acc then acc else IntMap.add k v acc
  ) s1 r

let rec unify (a : ty) (b : ty) (s : subst) : (subst, unit) result =
  let a = apply_s s a in
  let b = apply_s s b in
  match a, b with
  | TVar id1, TVar id2 when id1 = id2 -> Ok s
  | TVar id, _ ->
    if occurs id b then Error ()
    else Ok (IntMap.add id b s)
  | _, TVar id ->
    if occurs id a then Error ()
    else Ok (IntMap.add id a s)
  | TCon n1, TCon n2 ->
    if n1 = n2 then Ok s else Error ()
  | TArr (l1, r1), TArr (l2, r2) ->
    (match unify l1 l2 s with
     | Error () -> Error ()
     | Ok s1 -> unify r1 r2 s1)
  | _ -> Error ()

type ex =
  | EAtom of string
  | ELam of string * ex
  | EApp of ex * ex
  | ELet of string * ex * ex
  | EIf of ex * ex * ex
  | EBin of string * ex * ex

let parse_line (line : string) : ex option =
  let p = ref 0 in
  let len = String.length line in
  let bad = ref false in
  let sp () =
    while !p < len && (let c = line.[!p] in c = ' ' || c = '\t' || c = '\r' || c = '\n') do
      incr p
    done
  in
  let atom () =
    sp ();
    let q = ref !p in
    while !q < len && (let c = line.[!q] in c <> ' ' && c <> '\t' && c <> '\r' && c <> '\n' && c <> '(' && c <> ')') do
      incr q
    done;
    let r = String.sub line !p (!q - !p) in
    p := !q;
    r
  in
  let rec expr () : ex option =
    sp ();
    if !p >= len then ( bad := true; None )
    else if line.[!p] = '(' then (
      incr p;
      sp ();
      let kw = atom () in
      if kw = "lam" then (
        sp ();
        let v = atom () in
        match expr () with
        | None -> bad := true; None
        | Some b ->
          sp ();
          if !p >= len || line.[!p] <> ')' then ( bad := true; None )
          else ( incr p; Some (ELam (v, b)) )
      ) else if kw = "app" then (
        match expr () with
        | None -> bad := true; None
        | Some f ->
          match expr () with
          | None -> bad := true; None
          | Some x ->
            sp ();
            if !p >= len || line.[!p] <> ')' then ( bad := true; None )
            else ( incr p; Some (EApp (f, x)) )
      ) else if kw = "let" then (
        sp ();
        let v = atom () in
        match expr () with
        | None -> bad := true; None
        | Some val_e ->
          match expr () with
          | None -> bad := true; None
          | Some bo ->
            sp ();
            if !p >= len || line.[!p] <> ')' then ( bad := true; None )
            else ( incr p; Some (ELet (v, val_e, bo)) )
      ) else if kw = "if" then (
        match expr () with
        | None -> bad := true; None
        | Some c ->
          match expr () with
          | None -> bad := true; None
          | Some t ->
            match expr () with
            | None -> bad := true; None
            | Some e ->
              sp ();
              if !p >= len || line.[!p] <> ')' then ( bad := true; None )
              else ( incr p; Some (EIf (c, t, e)) )
      ) else if kw = "+" || kw = "==" then (
        match expr () with
        | None -> bad := true; None
        | Some x ->
          match expr () with
          | None -> bad := true; None
          | Some y ->
            sp ();
            if !p >= len || line.[!p] <> ')' then ( bad := true; None )
            else ( incr p; Some (EBin (kw, x, y)) )
      ) else ( bad := true; None )
    ) else (
      let a = atom () in
      if a = "" then ( bad := true; None )
      else Some (EAtom a)
    )
  in
  let res = expr () in
  sp ();
  if !bad || !p <> len then None else res

type scheme = { qs : int list; t : ty }
type env = scheme StringMap.t

let freshc = ref 0
let fresh () =
  let v = !freshc in
  incr freshc;
  TVar v

let ftv_env (e : env) : IntSet.t =
  StringMap.fold (fun _ sc acc ->
    let s = ftv sc.t IntSet.empty in
    let qs_set = List.fold_left (fun a x -> IntSet.add x a) IntSet.empty sc.qs in
    IntSet.fold (fun v a -> if not (IntSet.mem v qs_set) then IntSet.add v a else a) s acc
  ) e IntSet.empty

let inst (sc : scheme) : ty =
  let s = List.fold_left (fun acc v -> IntMap.add v (fresh ()) acc) IntMap.empty sc.qs in
  apply_s s sc.t

let gen (e : env) (t : ty) : scheme =
  let a = ftv t IntSet.empty in
  let b = ftv_env e in
  let qs = IntSet.elements (IntSet.diff a b) in
  { qs; t }

let apply_env (s : subst) (e : env) : env =
  StringMap.map (fun sc -> { qs = sc.qs; t = apply_s s sc.t }) e

let is_digit_str s =
  String.length s > 0 &&
  let ok = ref true in
  for i = 0 to String.length s - 1 do
    if s.[i] < '0' || s.[i] > '9' then ok := false
  done;
  !ok

let rec infer (env : env) (e : ex) : (subst * ty, unit) result =
  match e with
  | EAtom a ->
    if is_digit_str a then
      Ok (IntMap.empty, TCon "int")
    else if a = "T" || a = "F" then
      Ok (IntMap.empty, TCon "bool")
    else (
      match StringMap.find_opt a env with
      | Some sc -> Ok (IntMap.empty, inst sc)
      | None -> Error ()
    )
  | ELam (v, b) ->
    let beta = fresh () in
    let e2 = StringMap.add v { qs = []; t = beta } env in
    (match infer e2 b with
     | Error () -> Error ()
     | Ok (s1, t1) -> Ok (s1, TArr (apply_s s1 beta, t1)))
  | EApp (f, x) ->
    (match infer env f with
     | Error () -> Error ()
     | Ok (s1, t1) ->
       let e2 = apply_env s1 env in
       (match infer e2 x with
        | Error () -> Error ()
        | Ok (s2, t2) ->
          let beta = fresh () in
          let u1 = apply_s s2 t1 in
          let u2 = TArr (t2, beta) in
          (match unify u1 u2 IntMap.empty with
           | Error () -> Error ()
           | Ok s3 ->
             let t12 = comp_s s2 s1 in
             Ok (comp_s s3 t12, apply_s s3 beta))))
  | ELet (v, val_e, bo) ->
    (match infer env val_e with
     | Error () -> Error ()
     | Ok (s1, t1) ->
       let e2 = apply_env s1 env in
       let sc = gen e2 t1 in
       let e2 = StringMap.add v sc e2 in
       (match infer e2 bo with
        | Error () -> Error ()
        | Ok (s2, t2) -> Ok (comp_s s2 s1, t2)))
  | EIf (c, t, e2) ->
    (match infer env c with
     | Error () -> Error ()
     | Ok (s1, tc) ->
       (match unify tc (TCon "bool") s1 with
        | Error () -> Error ()
        | Ok sa ->
          let e_env2 = apply_env sa env in
          (match infer e_env2 t with
           | Error () -> Error ()
           | Ok (s2, tt) ->
             let e_env3 = apply_env s2 e_env2 in
             (match infer e_env3 e2 with
              | Error () -> Error ()
              | Ok (s3, te) ->
                let u1 = apply_s s3 tt in
                (match unify u1 te IntMap.empty with
                 | Error () -> Error ()
                 | Ok sb ->
                   let t1 = comp_s s3 s2 in
                   let t2 = comp_s t1 sa in
                   Ok (comp_s sb t2, apply_s sb te))))))
  | EBin (op, x, y) ->
    (match infer env x with
     | Error () -> Error ()
     | Ok (s1, t1) ->
       (match unify t1 (TCon "int") s1 with
        | Error () -> Error ()
        | Ok sa ->
          let e2 = apply_env sa env in
          (match infer e2 y with
           | Error () -> Error ()
           | Ok (s2, t2) ->
             (match unify t2 (TCon "int") s2 with
              | Error () -> Error ()
              | Ok sb ->
                let c1 = comp_s sb s2 in
                let c2 = comp_s c1 sa in
                let ret_ty = if op = "+" then TCon "int" else TCon "bool" in
                Ok (comp_s c2 s1, ret_ty)))))

let rec collect (t : ty) (acc : int list) : int list =
  match t with
  | TVar id -> if List.mem id acc then acc else acc @ [id]
  | TCon _ -> acc
  | TArr (l, r) -> collect r (collect l acc)

let rec show_t (t : ty) (nm : string IntMap.t) : string =
  match t with
  | TVar id -> (match IntMap.find_opt id nm with Some s -> s | None -> "?")
  | TCon s -> s
  | TArr (l, r) -> "(" ^ show_t l nm ^ " -> " ^ show_t r nm ^ ")"

let () =
  let lines = ref [] in
  (try
     while true do
       lines := input_line stdin :: !lines
     done
   with End_of_file -> ());
  let lines = Array.of_list (List.rev !lines) in
  let num_lines = Array.length lines in
  let idx = ref 0 in
  while !idx < num_lines && String.trim lines.(!idx) = "" do
    incr idx
  done;
  if !idx < num_lines then (
    let t_cases = int_of_string (String.trim lines.(!idx)) in
    incr idx;
    for _ = 1 to t_cases do
      while !idx < num_lines && String.trim lines.(!idx) = "" do
        incr idx
      done;
      if !idx >= num_lines then
        Printf.printf "HM TYPE-ERROR\n"
      else (
        let line = String.trim lines.(!idx) in
        incr idx;
        match parse_line line with
        | None -> Printf.printf "HM TYPE-ERROR\n"
        | Some e ->
          freshc := 0;
          (match infer StringMap.empty e with
           | Error () -> Printf.printf "HM TYPE-ERROR\n"
           | Ok (s, t) ->
             let f = apply_s s t in
             let vs = collect f [] in
             let nm = List.mapi (fun i vid ->
               (vid, String.make 1 (Char.chr (Char.code 'a' + i)))
             ) vs in
             let nm_map = List.fold_left (fun acc (k, v) -> IntMap.add k v acc) IntMap.empty nm in
             Printf.printf "HM %s\n" (show_t f nm_map))
      )
    done
  )
