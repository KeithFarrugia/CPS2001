(* include lang  *)

type state = int;;

type 'a set = 'a list;;

type 'a option = 
  | None 
  | Some of 'a;;


let tr_1 = [((0, A), 1); ((0, B), 0); ((1, A), 0); ((1, B), 1)];;
let s_1 = [A; B; B; A; A ;B ];;
let s_2 = [A; B; B; A; B ];;


type dfa = { 
      start: state;
      final: state set;
      delta: ((state * alpha) * state) set 
      };;


let create_dfsa (fin : state set) (tra : ((state * alpha) * state) set): dfa =
      {
        delta = tra;
        start = 0;  (* Always assumed to be 0 *)
        final = fin
      };;


let d_1 = create_dfsa [1] tr_1;; 






let rec transition (status: (state * alpha)) (tr_fun : ((state * alpha) * state) set) : state option =
(* ---------------- edit here begin ---------------- *)
  match tr_fun with
  | [] -> None
  | ((current_state , a) , next_state)::t -> if (status = (current_state, a)) then
                                                Some next_state
                                              else
                                                transition status t;;
(* ----------------- edit here end ----------------- *)




let rec delta_star (s: astring) (q: state) (d: dfa) : state option =
(* ---------------- edit here begin ---------------- *)
  match s with
  | [] -> Some q
  | a::t -> let transition_val = transition (q,a) d.delta in
            match transition_val with
            | None -> None
            | Some q -> delta_star t q d;;
(* ----------------- edit here end ----------------- *)




let is_accepted (s: astring) (d: dfa): bool = 
(* ---------------- edit here begin ---------------- *)
  let delta_star_value = delta_star s d.start d in
    let rec has_end_state (x:int) (states:state set) = 
      match states with
      | []   -> false
      | a::t -> a=x || has_end_state x t in

      match delta_star_value with
      | None -> false
      | Some (x:int) -> has_end_state x d.final;;
(* ----------------- edit here end ----------------- *)




let in_range (range: state) (d : dfa) : bool  =
(* ---------------- edit here begin ---------------- *)
  let delta_to_state ((q, _),_) = q in 
    let states = map delta_to_state d.delta in
      let check_range acc state = acc && (state >= 0 && state <= range) in
        reduce check_range true states;;

(* ----------------- edit here end ----------------- *)
  







 


(* Probably need to use reduce for this *)

(* let rec in_range (range: state) (tr_fun : ((state * alpha) * state) list) : bool  =
  map (map_in_range range) tr_fun;; *)
  (* match tr_fun with
  | []                   -> true
  | ((q1, _), q2) :: tr' -> ((state_in_range range q1) && (state_in_range range q2) && (in_range range tr'));;  *)
