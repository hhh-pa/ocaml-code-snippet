module StringSet = Set.Make(String)

let combine_pos a b =
  let len = String.length a in
  let diff = ref (-1) in
  let ok = ref true in
  for i = 0 to len - 1 do
    if !ok then begin
      let ca = a.[i] in
      let cb = b.[i] in
      if ca <> cb then begin
        if ca = '-' || cb = '-' then ok := false
        else if !diff >= 0 then ok := false
        else diff := i
      end
    end
  done;
  if !ok then !diff else -1

let covers imp mt =
  let len = String.length imp in
  let ok = ref true in
  for i = 0 to len - 1 do
    if imp.[i] <> '-' && imp.[i] <> mt.[i] then ok := false
  done;
  !ok

let bits_of m n =
  String.init n (fun i ->
    let bit = (m lsr (n - 1 - i)) land 1 in
    if bit = 1 then '1' else '0'
  )

let replace_char_at s idx ch =
  let b = Bytes.of_string s in
  Bytes.set b idx ch;
  Bytes.to_string b

let () =
  try
    let n = Scanf.scanf " %d" (fun x -> x) in
    let k = Scanf.scanf " %d" (fun x -> x) in
    let raw_mts = Array.init k (fun _ -> Scanf.scanf " %d" (fun x -> x)) in
    Array.sort compare raw_mts;
    let mts_list = ref [] in
    for i = 0 to k - 1 do
      if i = 0 || raw_mts.(i) <> raw_mts.(i - 1) then
        mts_list := raw_mts.(i) :: !mts_list
    done;
    let mts = Array.of_list (List.rev !mts_list) in
    let cur = ref (Array.init (Array.length mts) (fun i -> bits_of mts.(i) n)) in
    Array.sort compare !cur;
    let primes = ref [] in
    let rec find_primes () =
      let groups = Array.make (n + 1) [] in
      for i = 0 to Array.length !cur - 1 do
        let t = !cur.(i) in
        let c = ref 0 in
        for j = 0 to String.length t - 1 do
          if t.[j] = '1' then incr c
        done;
        groups.(!c) <- t :: groups.(!c)
      done;
      let used = ref StringSet.empty in
      let nxt = ref StringSet.empty in
      for g = 0 to n - 1 do
        List.iter (fun a ->
          List.iter (fun b ->
            let d = combine_pos a b in
            if d >= 0 then begin
              used := StringSet.add a (StringSet.add b !used);
              let c = replace_char_at a d '-' in
              nxt := StringSet.add c !nxt
            end
          ) groups.(g + 1)
        ) groups.(g)
      done;
      for i = 0 to Array.length !cur - 1 do
        let t = !cur.(i) in
        if not (StringSet.mem t !used) then
          primes := t :: !primes
      done;
      if not (StringSet.is_empty !nxt) then begin
        let next_list = StringSet.elements !nxt in
        cur := Array.of_list next_list;
        find_primes ()
      end
    in
    find_primes ();
    let primes_arr = Array.of_list !primes in
    Array.sort compare primes_arr;
    let unique_primes = ref [] in
    for i = 0 to Array.length primes_arr - 1 do
      if i = 0 || primes_arr.(i) <> primes_arr.(i - 1) then
        unique_primes := primes_arr.(i) :: !unique_primes
    done;
    let primes = List.rev !unique_primes in
    let num_mts = Array.length mts in
    let mtbits = Array.init num_mts (fun i -> bits_of mts.(i) n) in
    let unc = Array.make num_mts true in
    let cover = ref StringSet.empty in
    for i = 0 to num_mts - 1 do
      let cov = ref [] in
      List.iter (fun p ->
        if covers p mtbits.(i) then cov := p :: !cov
      ) primes;
      if List.length !cov = 1 then
        cover := StringSet.add (List.hd !cov) !cover
    done;
    let apply_cover () =
      for i = 0 to num_mts - 1 do
        if unc.(i) then begin
          let covered = ref false in
          StringSet.iter (fun p ->
            if not !covered && covers p mtbits.(i) then covered := true
          ) !cover;
          if !covered then unc.(i) <- false
        end
      done
    in
    apply_cover ();
    let rec greedy () =
      let any = ref false in
      for i = 0 to num_mts - 1 do
        if unc.(i) then any := true
      done;
      if !any then begin
        let best = ref "" in
        let bc = ref (-1) in
        List.iter (fun p ->
          let c = ref 0 in
          for i = 0 to num_mts - 1 do
            if unc.(i) && covers p mtbits.(i) then incr c
          done;
          if !c > !bc || (!c = !bc && (!bc < 0 || p < !best)) then begin
            best := p;
            bc := !c
          end
        ) primes;
        cover := StringSet.add !best !cover;
        apply_cover ();
        greedy ()
      end
    in
    greedy ();
    let covv = StringSet.elements !cover in
    Printf.printf "%d\n%d\n" (List.length primes) (List.length covv);
    List.iter (fun p ->
      Printf.printf "%s\n" p
    ) covv
  with End_of_file -> ()
