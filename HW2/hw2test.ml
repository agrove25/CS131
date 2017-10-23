let accept_empty derivation frag = match frag with
  | [] -> Some(derivation, frag)
  | _ -> None;;


(* This is a more generic test that tests the use an empty NT side, all the 
 * while making sure that the match does match from left to right *)

type test1_non_terminals = | S | A | B | C;;

let test1_rules = 
  [S, [N A; N B];
   S, [N A];
   S, [N B];
   A, [T 'a'; N B];
   A, [];
   B, [T 'b'];
   B, [T 'b'; N A];
   B, [T 'b'; N B];
  ];;

let test1_derivation =
  ( [ 
      (S, [N A; N B]);
      (A, []); 
      (B, [T 'b'; N B]);
      (B, [T 'b'; N A]); 
      (A, [T 'a'; N B]);
      (B, [T 'b']);         
  ] )

let test1_grammar = convert_grammar (S, test1_rules);;

let test_1 = (parse_prefix test1_grammar) accept_empty ['b'; 'b'; 'a'; 'b'] = 
  Some(test1_derivation, []);;

(* tests for a NT, T, NT combination and other odd loops*)

type test2_non_terminals = | S | A | B | C | D;;

let test2_rules =
   [S, [N B];
    S, [N C; T 'i'; N D];
    A, [T 'a'];
    A, [N A; N D];
    A, [N A; N C];
    B, [T 'b'];
    C, [T 'c'];
    C, [N A; T 'c'];
    D, [N B; T 'd'];
    D, [T 'd'; N B];
   ];;

let test2_derivation =
  ( [ 
      (S, [N C; T 'i'; N D]);
      (C, [N A; T 'c']);
      (A, [N A; N D]);
      (A, [T 'a']);
      (D, [T 'd'; N B]);
      (B, [T 'b']);
      (D, [N B; T 'd']);
      (B, [T 'b']);
  ] )

let test2_grammar = convert_grammar (S, test2_rules);;

let test_2 = (parse_prefix test2_grammar) accept_empty 
  ['a'; 'd'; 'b'; 'c'; 'i'; 'b'; 'd';] = Some(test2_derivation, []);;