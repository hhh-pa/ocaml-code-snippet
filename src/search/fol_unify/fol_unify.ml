module StringMap = Map.Make(String)

type term =
  | Var of string
  | Fn of string * term list

let parse_term s =
  let n = String.length s in
  let pos = ref 0 in
  let rec parse () =
    let q = ref !pos in
    while !q < n && (let c = s.[!q] in (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_') do
      incr q
    done;
    let sym = String.sub s !pos (!q - !pos) in
    pos := !q;
    if !pos < n && s.[!pos] = '(' then begin
      incr pos; (* skip '(' *)
      let args = ref [] in
      let stop = ref false in
      while not !stop do
        let arg = parse () in
        args := arg :: !args;
        if s.[!pos] = ',' then
          incr pos
        else begin
          incr pos; (* skip ')' *)
          stop := true
        end
      done;
      Fn (sym, List.rev !args)
    end else begin
      let isvar = sym.[0] >= 'a' && sym.[0] <= 'z' in
      if isvar then Var sym
      else Fn (sym, [])
    end
  in
  parse ()

let rec show = function
  | Var s -> s
  | Fn (s, []) -> s
  | Fn (s, args) ->
      s ^ "(" ^ String.concat "," (List.map show args) ^ ")"

let rec subst t th =
  match t with
  | Var v ->
      begin match StringMap.find_opt v !th with
      | None -> Var v
      | Some t' ->
          let r = subst t' th in
          th := StringMap.add v r !th;
          r
      end
  | Fn (sym, args) ->
      Fn (sym, List.map (fun a -> subst a th) args)

let rec occurs v t th =
  let s = subst t th in
  match s with
  | Var v' -> v' = v
  | Fn (_, args) -> List.exists (fun a -> occurs v a th) args

let rec unify a b th =
  let a = subst a th in
  let b = subst b th in
  match (a, b) with
  | (Var v1, Var v2) when v1 = v2 -> true
  | (Var v, _) ->
      if occurs v b th then false
      else begin
        th := StringMap.add v b !th;
        true
      end
  | (_, Var v) ->
      if occurs v a th then false
      else begin
        th := StringMap.add v a !th;
        true
      end
  | (Fn (f1, args1), Fn (f2, args2)) ->
      if f1 <> f2 || List.length args1 <> List.length args2 then false
      else
        List.for_all2 (fun a1 a2 -> unify a1 a2 th) args1 args2

let solve_case () =
  let s1 = Scanf.scanf " %s" (fun s -> s) in
  let s2 = Scanf.scanf " %s" (fun s -> s) in
  let t1 = parse_term s1 in
  let t2 = parse_term s2 in
  let th = ref StringMap.empty in
  let ok = unify t1 t2 th in
  if not ok then
    Printf.printf "UNIFY FAIL\n"
  else begin
    Printf.printf "UNIFY";
    StringMap.iter (fun k v ->
      let r = subst v th in
      Printf.printf " %s=%s" k (show r)
    ) !th;
    Printf.printf "\n"
  end

let () =
  try
    let t = Scanf.scanf " %d" (fun x -> x) in
    for _ = 0 to t - 1 do
      solve_case ()
    done
  with End_of_file -> ()
