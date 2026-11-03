type node = {
  stones: int;
  player: int;
  mutable visits: int;
  mutable wins: int;
  moves: int array;
  mutable kids: int array;
  mutable nkids: int;
}

let create_node stones player k =
  let max_moves = min k stones in
  let moves = Array.init max_moves (fun i -> i + 1) in
  {
    stones;
    player;
    visits = 0;
    wins = 0;
    moves;
    kids = Array.make max_moves (-1);
    nkids = 0;
  }

let lcg_mult = 6364136223846793005L
let lcg_add = 1442695040888963407L

let () =
  try
    let t_cases = Scanf.scanf " %d" (fun x -> x) in
    for _ = 1 to t_cases do
      let n = Scanf.scanf " %d" (fun x -> x) in
      let k = Scanf.scanf " %d" (fun x -> x) in
      let iters = Scanf.scanf " %d" (fun x -> x) in
      let seed = ref (Scanf.scanf " %Ld" (fun x -> x)) in

      let lcg_next () =
        seed := Int64.add (Int64.mul !seed lcg_mult) lcg_add;
        !seed
      in
      let lcg_next_int m =
        let r = lcg_next () in
        let rem = Int64.unsigned_rem r (Int64.of_int m) in
        Int64.to_int rem
      in

      let tree_cap = ref 1024 in
      let tree = ref (Array.make !tree_cap (create_node 0 0 0)) in
      let tree_size = ref 0 in
      let add_node nd =
        if !tree_size = !tree_cap then begin
          let new_cap = !tree_cap * 2 in
          let new_tree = Array.make new_cap (create_node 0 0 0) in
          Array.blit !tree 0 new_tree 0 !tree_size;
          tree := new_tree;
          tree_cap := new_cap
        end;
        !tree.(!tree_size) <- nd;
        incr tree_size
      in

      let root = create_node n 0 k in
      add_node root;

      for _ = 1 to iters do
        let v = ref 0 in
        let traj = ref [0] in
        let selection_done = ref false in
        while not !selection_done do
          let nd = !tree.(!v) in
          if nd.stones = 0 then
            selection_done := true
          else if nd.nkids < Array.length nd.moves then begin
            let mv = nd.moves.(nd.nkids) in
            let ch = create_node (nd.stones - mv) (1 - nd.player) k in
            let ch_idx = !tree_size in
            add_node ch;
            nd.kids.(nd.nkids) <- ch_idx;
            nd.nkids <- nd.nkids + 1;
            v := ch_idx;
            traj := ch_idx :: !traj;
            selection_done := true
          end else begin
            let l_n = log (float_of_int nd.visits) in
            let bi = ref 0 in
            let bu = ref (-1e300) in
            for i = 0 to nd.nkids - 1 do
              let c = !tree.(nd.kids.(i)) in
              let c_wins = float_of_int c.wins in
              let c_visits = float_of_int c.visits in
              let u = 1.0 -. c_wins /. c_visits +. sqrt (2.0 *. l_n /. c_visits) in
              if u > !bu then begin
                bu := u;
                bi := i
              end
            done;
            v := nd.kids.(!bi);
            traj := !v :: !traj
          end
        done;

        let st = ref !tree.(!v).stones in
        let pl = ref !tree.(!v).player in
        while !st > 0 do
          let nm = min k !st in
          let pick = lcg_next_int nm in
          let mv = pick + 1 in
          st := !st - mv;
          pl := 1 - !pl
        done;

        let winner = 1 - !pl in
        List.iter (fun idx ->
          !tree.(idx).visits <- !tree.(idx).visits + 1;
          if !tree.(idx).player = winner then
            !tree.(idx).wins <- !tree.(idx).wins + 1
        ) !traj
      done;

      let rt = !tree.(0) in
      let best = ref rt.moves.(0) in
      let bv = ref (-1) in
      for i = 0 to rt.nkids - 1 do
        let c_idx = rt.kids.(i) in
        let c = !tree.(c_idx) in
        let mv = rt.stones - c.stones in
        if c.visits > !bv then begin
          bv := c.visits;
          best := mv
        end
      done;

      Printf.printf "MCTS %d V" !best;
      for i = 0 to rt.nkids - 1 do
        Printf.printf " %d" !tree.(rt.kids.(i)).visits
      done;
      Printf.printf " W";
      for i = 0 to rt.nkids - 1 do
        Printf.printf " %d" !tree.(rt.kids.(i)).wins
      done;
      Printf.printf "\n"
    done
  with End_of_file -> ()
