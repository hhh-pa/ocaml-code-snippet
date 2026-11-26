(* Indexing and ranking (NR 8.4). *)

let indexx arr =
  let m = 7 and nstack = 64 in
  let n = Array.length arr in
  let istack = Array.make nstack 0 in
  let indx = Array.init n (fun i -> i) in
  let ir = ref (n - 1) and jstack = ref (-1) and l = ref 0 in
  let cont = ref true in
  while !cont do
    if !ir - !l < m then (
      let j = ref (!l + 1) in
      while !j <= !ir do
        let indxt = indx.(!j) and a = arr.(indx.(!j)) in
        let i = ref (!j - 1) in
        while !i >= !l && arr.(indx.(!i)) > a do
          indx.(!i + 1) <- indx.(!i);
          i := !i - 1
        done;
        indx.(!i + 1) <- indxt;
        j := !j + 1
      done;
      if !jstack < 0 then cont := false
      else (
        ir := istack.(!jstack);
        jstack := !jstack - 1;
        l := istack.(!jstack);
        jstack := !jstack - 1))
    else (
      let k = (!l + !ir) lsr 1 in
      let t = indx.(k) in
      indx.(k) <- indx.(!l + 1);
      indx.(!l + 1) <- t;
      (if arr.(indx.(!l)) > arr.(indx.(!ir)) then (
         let t = indx.(!l) in
         indx.(!l) <- indx.(!ir);
         indx.(!ir) <- t));
      (if arr.(indx.(!l + 1)) > arr.(indx.(!ir)) then (
         let t = indx.(!l + 1) in
         indx.(!l + 1) <- indx.(!ir);
         indx.(!ir) <- t));
      (if arr.(indx.(!l)) > arr.(indx.(!l + 1)) then (
         let t = indx.(!l) in
         indx.(!l) <- indx.(!l + 1);
         indx.(!l + 1) <- t));
      let i = ref (!l + 1) and j = ref !ir in
      let indxt = indx.(!l + 1) and a = arr.(indx.(!l + 1)) in
      let inner = ref true in
      while !inner do
        (let c = ref true in
         while !c do
           i := !i + 1;
           if not (arr.(indx.(!i)) < a) then c := false
         done);
        (let c = ref true in
         while !c do
           j := !j - 1;
           if not (arr.(indx.(!j)) > a) then c := false
         done);
        if !j < !i then inner := false
        else (
          let t = indx.(!i) in
          indx.(!i) <- indx.(!j);
          indx.(!j) <- t)
      done;
      indx.(!l + 1) <- indx.(!j);
      indx.(!j) <- indxt;
      jstack := !jstack + 2;
      if !ir - !i + 1 >= !j - !l then (
        istack.(!jstack) <- !ir;
        istack.(!jstack - 1) <- !i;
        ir := !j - 1)
      else (
        istack.(!jstack) <- !j - 1;
        istack.(!jstack - 1) <- !l;
        l := !i))
  done;
  indx

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
  if op = "INDEX" || op = "RANK" then (
    let n = int_of_string toks.(1) in
    let arr = Array.init n (fun i -> int_of_string toks.(2 + i)) in
    let indx = indexx arr in
    let out =
      if op = "INDEX" then Array.to_list indx
      else (
        let r = Array.make n 0 in
        for j = 0 to n - 1 do
          r.(indx.(j)) <- j
        done;
        Array.to_list r)
    in
    print_endline (String.concat " " (List.map string_of_int out)))
