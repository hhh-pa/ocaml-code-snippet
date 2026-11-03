type role = Follower | Candidate | Leader

type raft_node = {
  id: int;
  mutable role: role;
  mutable current_term: int;
  mutable voted_for: int;
  mutable log: (int * string) list;
  mutable log_len: int;
  mutable commit_index: int;
}

let () =
  try
    let n_nodes = Scanf.scanf " %d" (fun x -> x) in
    let n_events = Scanf.scanf " %d" (fun x -> x) in
    let cluster = Array.init n_nodes (fun i -> {
      id = i;
      role = Follower;
      current_term = 0;
      voted_for = -1;
      log = [];
      log_len = 0;
      commit_index = -1;
    }) in
    let majority = n_nodes / 2 + 1 in
    for _ = 0 to n_events - 1 do
      let event_type = Scanf.scanf " %s" (fun x -> x) in
      if event_type = "START_ELECTION" then begin
        let cand_id = Scanf.scanf " %d" (fun x -> x) in
        let cand = cluster.(cand_id) in
        cand.role <- Candidate;
        cand.current_term <- cand.current_term + 1;
        cand.voted_for <- cand_id;
        let votes = ref 1 in
        for i = 0 to n_nodes - 1 do
          if i <> cand_id then begin
            let peer = cluster.(i) in
            if peer.current_term < cand.current_term then begin
              peer.current_term <- cand.current_term;
              peer.role <- Follower;
              peer.voted_for <- cand_id;
              incr votes
            end
          end
        done;
        if !votes >= majority then begin
          cand.role <- Leader;
          Printf.printf "LEADER ELECTED: %d TERM: %d\n" cand_id cand.current_term
        end else begin
          cand.role <- Follower;
          Printf.printf "ELECTION FAILED: %d\n" cand_id
        end
      end else if event_type = "APPEND_ENTRY" then begin
        let leader_id = Scanf.scanf " %d" (fun x -> x) in
        let cmd = Scanf.scanf " %s" (fun x -> x) in
        let leader = cluster.(leader_id) in
        if leader.role <> Leader then
          Printf.printf "REJECT NOT LEADER\n"
        else begin
          leader.log <- (leader.current_term, cmd) :: leader.log;
          leader.log_len <- leader.log_len + 1;
          let replicated = ref 1 in
          for i = 0 to n_nodes - 1 do
            if i <> leader_id then begin
              let peer = cluster.(i) in
              peer.current_term <- leader.current_term;
              peer.role <- Follower;
              peer.log <- (leader.current_term, cmd) :: peer.log;
              peer.log_len <- peer.log_len + 1;
              incr replicated
            end
          done;
          if !replicated >= majority then begin
            leader.commit_index <- leader.log_len - 1;
            for i = 0 to n_nodes - 1 do
              cluster.(i).commit_index <- leader.commit_index
            done;
            Printf.printf "COMMITTED %s AT INDEX %d\n" cmd leader.commit_index
          end
        end
      end else if event_type = "PRINT_STATE" then begin
        let node_id = Scanf.scanf " %d" (fun x -> x) in
        let node = cluster.(node_id) in
        let r_str = match node.role with
          | Leader -> "LEADER"
          | Candidate -> "CANDIDATE"
          | Follower -> "FOLLOWER"
        in
        Printf.printf "NODE %d ROLE %s TERM %d COMMITTED %d\n" node_id r_str node.current_term node.commit_index
      end
    done
  with End_of_file -> ()
