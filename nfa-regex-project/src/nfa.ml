open Sets
open List

(*********)
(* Types *)
(*********)

type ('q, 's) transition = 'q * 's option * 'q

type ('q, 's) nfa_t = {
  sigma: 's list;
  qs: 'q list;
  q0: 'q;
  fs: 'q list;
  delta: ('q, 's) transition list;
}

(***********)
(* Utility *)
(***********)

(* explode converts a string to a character list *)
let explode (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) []

(****************)
(* Part 1: NFAs *)
(****************)

let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list =
let rec move_helper lst qs s = match lst with
| [] -> []
| (a, b, c)::t -> if elem a qs && b = s then insert c (move_helper t qs s) else move_helper t qs s
in List.sort_uniq Stdlib.compare (move_helper nfa.delta qs s)

let e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list =
  let rec helper visited curr_states =
    let next_states = List.fold_left (fun acc (q1, sym, q2) ->
      if List.mem q1 curr_states && sym = None && not (List.mem q2 visited)
      then q2 :: acc
      else acc
    ) [] nfa.delta in
    if next_states = []
    then visited
    else helper (visited @ next_states) next_states
  in
  helper qs qs |> List.sort_uniq Stdlib.compare

let accept (nfa: ('q,char) nfa_t) (s: string) : bool = 
let lst = explode s in 
let rec accept_helper state char_lst = match char_lst with
| [] -> List.exists(fun s -> (List.mem s nfa.fs)) state
| h::t -> let next = (e_closure nfa (move nfa state (Some h))) in accept_helper next t in
accept_helper (e_closure nfa [nfa.q0]) lst

(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
List.map (fun s -> e_closure nfa (move nfa (e_closure nfa qs) (Some s))) nfa.sigma

let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =
List.map (fun s -> (qs, (Some s), (e_closure nfa (move nfa (e_closure nfa qs) (Some s))))) nfa.sigma

let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
if intersection qs nfa.fs != [] then [qs] else []

let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list) : ('q list, 's) nfa_t =
match work with
| [] -> dfa
| h::t -> let states = (new_states nfa h) in let trans = (new_trans nfa h) in let finals = (new_finals nfa h) in
let new_work = diff (union t states) dfa.qs in 
let new_dfa = {
sigma = nfa.sigma;
qs = insert h dfa.qs;
delta = (union dfa.delta trans);
q0 = dfa.q0;
fs = (union dfa.fs finals)
} 
in if (List.mem h dfa.qs) then (nfa_to_dfa_step nfa dfa new_work) else (nfa_to_dfa_step nfa new_dfa new_work)

let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t =
let dfa = 
{
sigma = nfa.sigma;
qs = [];
delta = [];
q0 = (e_closure nfa [nfa.q0]);
fs = []
}  
in nfa_to_dfa_step nfa dfa [dfa.q0]