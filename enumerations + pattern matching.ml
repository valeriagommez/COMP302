(* Enumerations *)

(* This creates a brand new type, NOT just declaring a name for a pre-existing one *)

type hand = Rock | Paper | Scissors (* Have to be capitalized *) 
                                                                 

(* We can't have two enumerations with the same type (ex : Rock inside an 'elements' enum *)

let my_hand = Rock
let your_hand = Paper
  
let beats (h1 : hand) (h2 : hand) : bool = 
  (* 
     Instead of using if statements, use "match" : 
     This looks a lot like "switch" statements we learned in C
  *)
  match h1 with 
  (* We can also use nested pattern matching : *)
  | Rock -> 
      (match h2 with 
       | Scissors -> true
       | _ -> false)
      
  | Paper ->  
      (match h2 with 
       | Scissors -> true
       | _ -> false)
      
  | Scissors -> 
      (match h2 with 
       | Scissors -> true
       | _ -> false)
      
      
type outcome = W | D | L (* with the perspective of P1 *)

let play (h1 : hand) (h2 : hand) : outcome = 
  
  if h1 = h2 then D 
  
  else
    (* Create a tuple to match h1 and h2 ! *)
    match (h1, h2) with 
    | (Rock, Scissors) -> W 
    | (Scissors, Paper) -> W
    | (Paper, Rock) -> W
    | _ -> L (* Everything else is a lie *)
  
(* playing Scissors Rock : 
   1. Scissors != Rock --> NOT D
   2. (Scissors, Rock) matches NO tuples except for the last one ( '_' ) --> L
*)
      
(* We need to make matches for all possible scenarios *)
