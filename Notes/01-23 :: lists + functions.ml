(* -------------- Lists -------------- *)

let ones = [1;1;1]
           
let rec append (l1 : 'a list) (l2 : 'a list) : 'a list = 
  match l1 with
  | [] -> l2
  | x :: xs -> 
      x :: append xs l2
        
  
   (* append (1, 2) L 
   
---> append (1 :: (2 :: [])) L 
---> 1 :: append (2 :: []) L
---> 1 :: (2 :: append [] L) 
---> 1 :: (2 :: L)   

*)

let append1 (l : 'a list) (x : 'a) : 'a list = 
  append l (x :: []) (* OR simply append l [x] *)
  
let rev l = 
  match l with 
  | [] -> []
  | x :: xs -> append1 (rev xs) x 
                                  
(* rev takes O(n^2) because it refers to append1 (which is also linear BUT
append1 also calls append at EACH element of l) 

We should be able to reverse a list in LINEAR time! 
- Think of lists as stacks 
- Start with an empty stack, and unstack l such that the top now becomes the bottom*)

let rec rev_linear l = 
  let rec go l acc = 
    match l with 
    | [] -> acc
    | x :: xs -> go xs (x :: acc) (* Tail recursive! *)
  in go l []
    
    
  
(* -------------- Functions -------------- *)

let twice f x = f (f x)

    
(*   

let id x = x   IS EQUAL TO   let id = fun x -> x   
- we can name an anonymous function using the keyword 'fun' 


let const y x = y   ==  let const y = fun x -> y  
                    ==  let const = fun y -> fun x -> y
                    
- every function that takes more than one paremeter is of higher order
  - a function that's inside a function
- const applied to one input (y) returns a function called in another function (x)

--> (const 3) "Hello"
--> (fun x -> 3) "Hello"
--> 3

*)


let rec times (n : int) (f : 'a -> 'a) (x : 'a) : 'a = 
  if n = 0 then x
  else f (times (n - 1) f x) 
                             
(* 

This function is NOT tail recursive 
- the return type of times is the same as the type of x

*)
                             
                             



