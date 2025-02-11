let product_tr l return_outside =
  let rec go l return =
    match l with
    | [] -> return 1 
    | x :: xs ->
        if x = 0 then return_outside 0
        else go xs (fun n -> return (n*x) ) 
  in go l return_outside
    
    
exception SawZero (* capital letter ==> it's a constructor! *)
                  
let product l = 
  let rec go l = 
    match l with 
    | [] -> 1
    | x :: xs ->
        if x = 0 then raise SawZero
        else x * go xs
  in go l
            
            
(* 

raise : exn (exception) -> 'a 
  - it returns ANYTGHING, however, it never actually returns 

product [2;4;8;0] ;; ==> Exception: SawZero.
  - However, we want it to just return 0!
   
*)

let product l = 
  let rec go l = 
    match l with 
    | [] -> 1
    | x :: xs ->
        if x = 0 then raise SawZero
        else x * go xs
  in 
  try go l with
  | SawZero -> 0
  
    
(* 

try : 
    return go l
else SawZero: 
    return 0
   
*)

type 'a tree = Empty | Node of 'a tree * 'a * 'a tree
                   
exception NotFound 
    
(* Finding the first element satisfying p : *)
let rec find (p : 'a -> bool) (t : 'a tree) : 'a =
  match t with 
  | Empty -> raise NotFound
  | Node (l, x, r) ->
      if p x then x
      else 
        try find p l with   (* searching through left subtree *)
        | NotFound -> find p r (* if that fails, then searching through right subtree *)
  
                        
(* 

CHANGE MAKING PROBLEM 

- Try to find a selection of coins to add up to a certain target amount 

Pathological currency : 
- 7 cents ; 2 cents
- Making change for 15 cents
   - Greedy algorithm does NOT work !
   - 15 - 2*7 = 1 ==> problem !

- We're going to choose the biggest coin we can every time UNLESS we encounter
  a problem, then we go back in time and Change our choice
  
*)

exception Change

let rec change (coins : int list) (amt : int) : int list = 
  match coins, amt with
  | _, 0 -> []
  | [], _ -> raise Change (* we need to backtrack! *)
  | c :: cs, amt ->
      if c > amt then change cs amt 
      else 
        try c :: change (c::cs) (amt - c) with (* we can use a coin multiple times *)
        | Change -> change cs amt (* if using c was bad, then we skip it *)
                                  
                                  
let patho = [7;2]
let canada = [100;25;10;5;1]
             
  
(* Trying it out ! (WITHOUT EXCEPTIONS) *)

let rec change (coins : int list) (amt : int) : int list options = 
  failwith "use options" 

let rec change (coins : int list) (amt : int) 
    (succeed : int list -> 'r) (fail : unit -> 'r) : 'r =
  match coins, amt with 
  | _, 0 -> succeed []
  | [], _ -> fail ()
  | c :: cs, amt -> 
      if c > amt then change cs amt succeed fail
      else c :: change (c::cs) (amt - c) 
             (fun solution -> succeed (c::solution))
             (fun () -> change cs amt succeed failure)










