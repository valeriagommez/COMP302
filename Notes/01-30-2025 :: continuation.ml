(* Continuation and types *) 

let product l return_outside = 
  let rec go l return =
    match l with 
    | [] -> return l
    | x :: xs -> 
        if x = 0 then return_outside 0 else
          go xs (fun y -> return (x * y)) 
  in go l return_outside
    
  
    
let rec fold_right f l e = 
  match l with 
  | [] -> e
  | x :: xs -> f x (fold_right f xs e)
                 
  
(*

fold_right f [1;2;3] 0  =  f 1 (f 2 (f 3 0))

  Exercise 1 : what do you choose for f to make the above calculate 
  the sun of the elements in the list
  - f x y -> x + y
     
  Exercise 1.5 : what do you choose for f to compute the LENGTH of the list?
  - f x y -> y + 1 
  
  Exercise 2 : what is the most general type of fold_right?
  - f 'a 'r -> 'r
  - fold_right (f 'a 'r) l e -> fold_right 'r l e -> 

*)
