let rec contains a b = match b with
  |  [] -> false
  |  h :: t -> if h = a then true else (contains a t);;

let rec subset a b = match a with
  |  [] -> true
  |  h :: t -> if (contains h b) then (subset t b) else false;;

let equal_sets a b = (subset a b) && (subset b a);;

let rec set_union a b = match a with
  |  [] -> b
  |  h :: t -> if (contains h b) then (set_union t b) 
                                 else (set_union t (h :: b));;

let rec set_intersection a b = match a with
  | [] -> []
  | h :: t -> if (contains h b) then h :: (set_intersection t b)
                                else (set_intersection t b);;

let rec set_diff a b = match a with
  |  [] -> []
  |  h :: t -> if (contains h b) then (set_diff t b)
                                 else h :: (set_diff t b);;

let rec computed_fixed_point eq f x = if (eq (f x) x)
  then x
  else (computed_fixed_point eq f (f x))

let rec computed_periodic_point eq f p x =
  let rec find_value f p x = 
    if (p = 0) then x else (find_value f (p-1) (f x)) in

  let new_value = find_value f p x in  
  if (eq new_value x) then x else (computed_periodic_point eq f p new_value);;

let rec while_away s p x = 
  if p x then x :: while_away s p (s x) else [];;

let rec rle_decode lp = match lp with
  | [] -> []
  | h :: t -> if (fst (h) == 0) then rle_decode t else snd (h) :: rle_decode ((fst (h) - 1, snd(h)) :: t)

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

let filter_blind_alleys g =
  (* symbol == symbol, terminals == symbol * (symbol list) list *)
  (* this function checks the symbol. If it is a terminal or has a path to a terminal, it'll return true *)
  let rec contains_symbol symbol terminals = match symbol with 
  | T symbol -> true
  | N symbol -> if (List.exists (fun x -> fst x = symbol) terminals) then true else false in

  (* rhs == symbol list, terminals == symbol * (symbol list) list *)
  (* checks if the function is the rhs is a terminal set *)
  let rec is_terminal rhs terminals = match rhs with
  |  [] -> true
  |  h :: t -> if (contains_symbol h terminals) then (is_terminal t terminals) else false in  

  (* rules == symbol * (symbol list) list == snd g, terminals == symbol * (symbol list) list *)
  (* checks if the rule is safe and not a duplicate, if so, it'll add that rule to the terminals (i.e. the same rules) *)
  let rec gen_terminals rules terminals = match rules with
  |  [] -> terminals
  |  h :: t -> if ((not (contains h terminals)) && (is_terminal (snd h) terminals))
               then gen_terminals t (h :: terminals) else gen_terminals t terminals in

  (* g_terminals = symbol * (symbol list) list == the list of non-BA rules *)
  let g_terminals = computed_fixed_point (=) (gen_terminals (snd g)) [] in  

  (* just filters the set of rules in such a way that preserves order *)
  (fst g, set_intersection (snd g) g_terminals);;