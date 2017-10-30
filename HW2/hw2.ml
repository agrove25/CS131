type ('terminal, 'nonterminal) symbol =
  | T of 'terminal
  | N of 'nonterminal;;

let rec prod_fun nt rules = match rules with
  | [] -> []
  | (fst, snd) :: t ->  if (fst = nt) 
                        then snd :: (prod_fun nt t)
                        else (prod_fun nt t);;  

let convert_grammar (start, rules) = (start, fun nt -> (prod_fun nt rules));; 

let rec matcher nt rules alt_list acc d frag =

  let rec checker rhs d frag = match rhs with
  | [] -> acc d frag
  | (N nterm) :: tail -> matcher nterm rules (rules nterm) (checker tail) d frag
  | (T term) :: tail -> match frag with
    | [] -> None
    | f :: s -> if (f = term)
                then checker tail d s
                else None in

  match alt_list with
    | [] -> None
    | h :: t -> match (checker h (d@[(nt, h)]) frag) with
      | None -> matcher nt rules t acc d frag
      | Some(a) -> Some(a);;

let parse_prefix (start, rules) = (fun acc frag -> matcher start rules (rules start) acc [] frag) 
