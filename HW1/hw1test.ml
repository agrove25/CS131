let subset_test0 = subset [1; 2;] [1; 2; 3;];;
let subset_test1 = not (subset [5; 2;] [1; 2; 3;]);;
let subset_test2 = not (subset [5; 2;] []);;

let equal_sets0 = equal_sets [5; 2; 4; 5; 2;] [2; 4; 5;];;
let equal_sets1 = not (equal_sets [5; 1; 3;] [2; 1; 3;]);;
let equal_sets1 = not (equal_sets [] [2; 1; 3;]);;
let equal_sets1 = not (equal_sets [5; 1; 3;] []);;

let set_union0 = equal_sets (set_union [1; 2; 3;] [1; 2; 3;]) [1; 2; 3;];;

let set_intersection0 = equal_sets (set_intersection [] [1; 2; 3;]) [];; 
let set_intersection1 = equal_sets (set_intersection [1; 2; 3;] []) [];;
let set_intersection2 = equal_sets (set_intersection [2; 1; 4;] [1; 2;]) [1; 2;];;

let set_diff0 = equal_sets (set_diff [2; 4;] [1; 2; 3;]) [4];;
let set_diff1 = equal_sets (set_diff [2; 3;] [1; 2; 3; 4;]) [];;

let computed_fixed_point_test0 =
  computed_fixed_point (=) (fun x -> x *. 4.) 1. = infinity;;
let computed_fixed_point_test1 =
  computed_fixed_point (=) (fun x -> x ** 2.) 0. = 0.;;

let computed_periodic_point_test0 =
  computed_periodic_point (=) (fun x -> x * 2) 0 (3) = 3;;
let computed_periodic_point_test1 =
  computed_periodic_point (=) (fun x -> x / 2) 5 1 = 0;;

let while_away0 = while_away ((+) 3) ((>) 10) 0 = [0; 3; 6; 9;];;
let while_away1 = while_away (fun x -> x * 2) ((>) 8191) 1 = 
	[1; 2; 4; 8; 16; 32; 64; 128; 256; 512; 1024; 2048; 4096;] ;;

let rledecode0 = rle_decode [(2,0); (1,6)] = [0; 0; 6;];;
let rledecode1 = rle_decode [(0, 3); (1, 2); (4, 1);] = [2; 1; 1; 1; 1;];;

type non_terminals = | S | A | B | C;;
let rules =
  [S, [N A; N B];
   S, [N A];
   S, [N B];
   A, [N C];
   A, [T 'a'; N C];
   A, [T 'a'; N B];
   A, [];
   B, [T 'b'];
   B, [T 'b'; N C];
   B, [T 'b'; N A];
   B, [T 'b'; N B];
   C, [N C];
  ]

let new_rules_S = 
  [S, [N A; N B];
   S, [N A];
   S, [N B];
   A, [T 'a'; N B];
   A, [];
   B, [T 'b'];
   B, [T 'b'; N A];
   B, [T 'b'; N B];
  ]

let filter_blind_alleys0 = filter_blind_alleys (S, rules) = (S, new_rules_S);;
let filter_blind_alleys1 = filter_blind_alleys (A, rules) = (A, new_rules_S);;

