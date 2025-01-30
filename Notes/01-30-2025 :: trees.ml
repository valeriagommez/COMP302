(* -------------------  TREES  ------------------- *)

type 'a tree =
  | Empty
  | Node of 'a tree * 'a * 'a tree
              
   
           
(* 

Finding the first element in a tree that satisfies a certain condition 
- p is the function defining the condition

*)
              
let rec find (p: 'a -> bool) (t : 'a tree) : 'a option = 
  match t with
  | Empty -> None
  | Node (l, x, r) ->
      if p x then Some x 
      else match find p l with (* start looking in the left subtree *)
        | Some y -> Some y
        | None -> find p r  (* if that fails, then look in the right subtree *)
                    
  
let rec find_tr (p: 'a -> bool) (t : 'a tree) 
    (return : 'a -> 'r) (fail : unit -> 'r) : 'r = 
  match t with 
  | Empty -> fail ()
  | Node (l, x, r) ->
      if p x then return x 
      else find_tr p l 
          return (* (fun y -> return y) --> no need to call dummy function that just relays the information to return *)
          (fun () -> find_tr p r return fail) (* (fun () -> find_tr p r (fun y -> return y) (fun () -> fail () )) *)
                    
                    
(* unit is like void in Java ! it's serving like a dummy input *) 
        
  (* 

for find_tr : more efficient because we're directly returning, not recursing back 

1. call find p l 

2. if it succeeds then we succeed and teturn the same thing  
  - fun y -> return y
  
3. if it fails, then we search on the right
  - fun () -> find_tr p r (fun y -> return y) (fun () -> fail () )

QUESTION : Do f and fun have the same signature?
f : 'a -> 'b then (fun x -> f x) : 'a -> 'b  (?) 

ANSWER : 
- let x : 'a
- then f x : 'b
- (fun x -> f y) x = f x : 'b
- they're the same!

*)

let ex = find_tr (fun x -> x <17)
    (Node 
       (
         Node (Empty, 18, Empty),
         19,
         Node (Empty, 14, Empty)
       )
    )
    (fun x -> x) (fun () -> -1) 
