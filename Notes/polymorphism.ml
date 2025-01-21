(* Polymorphism *)

let id x = x (* 'a --> 'a *)

let const x y = x (* 'a --> 'b --> 'a *)
                  
let const z y = 
  let _ = y + 1 in
  z (* 'a --> int --> 'a 
even if we're not using y in any way, we're still making it an 'int' 
since we're performing an operation on it *)
  

(* Going back to the UNO example from last class *)
    
type value = Num of int | Plus2 | Reverse | Skip
             
let y = Num 13
    
type value_result = Success of value | Failure
                   
let value_of_int n = 
  if 0 <= n && n <= 9 then Success (Num n) 
  else Failure (* we only get a success if the numbers are from 0-9 *)
               
let ex = match value_of_int 11 with 
  | Failure -> "oh no!"
  | Success (Num _) -> "yes"
    
  
type 'a option = Some of 'a | None (* Option<T> *)
                 
let success : string option = "hello" (* b' --> 'a --> 'b *)
let int_success : int option = Some 3   
                                      
    
(* None is a generic failure, it encompasses any type of failure *)

let failure : string option = None 
let int_failure : int option = None 

  
let value_of_int n : value option (* option of value 'value' *) = 
  if 0 <= n && n <= 9 
               
  then Some (Num n) (* Some (Num n) should be of type value option 
                    See type 'a option declaration, here, 'a is of type "Num"*)  
                    
  else None (* None is of type 'a option 
            'a option = value option WHEN 'a = value*)  
  
  
(* Other example *)
let value_of_int' n  = (* the return type will always be 'a option *)
  if 0 <= n && n <= 9 
  then None 
  else None 
           
    
(* The most generic type possible will be the one chosen for the return type *) 


(* Recursive types *)

type value = Num of int | Plus2 | Reverse | Skip
type color = R | G | Y | B
type card = color * value

            
(* A hand contains a hand as well, the base case is Empty *)
type hand = Empty | OneMore of card * hand    
                                           
(*  Green 3, Yellow Reverse *)                   
let h = OneMore ( (G, Num 3), OneMore ( (Y, Reverse), Empty) ) 
                                                               
(*  Blue Skip, Green 3, Yellow Reverse *)   
let h' = OneMore ( (B, Skip), h)
  
(* Making a generic list type *) 
let 'a mylist = Nil |Cons of 'a * ('a mylist)
                                  
type hand = card mylist (* a hand is just a list of cards *) 
  
(* Finding how many cards a hand contains *)   
let rec len (h : 'a mylist) : int =
  match h with
  | Empty -> 0
  | OneMore (_, h') -> 1 + len h' (* _ is used because 'a mylist is of type 'a 
                                  but we don't really care about it *)
                         
  
(* Making it tail recursive *)
let len_tr (h : 'a mylist) : int = 
  let rec len_tail_recursive (h : hand) (acc : int) : int = 
    match h with 
    | Empty -> acc
    | OneMore (_, h') -> len_tail_recursive h' (acc + 1)
  in 
  len_tail_recursive h 0
    
  







               
