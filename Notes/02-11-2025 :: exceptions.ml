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
                              
          












