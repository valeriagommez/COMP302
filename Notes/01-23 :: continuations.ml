(* Higher order accumulator *)

let rec append l1 l2 = 
  match l1 with 
  | [] -> l2 
  | x :: xs -> 
      let ys = append xs l2 
      in x :: ys


let rec append_tr (l1 : 'a list) (l2 : 'a list) (return : 'a list -> 'r) : 'r = 
  match l1 with 
  | [] -> return l2
  | x :: xs -> append_tr xs l2 (fun ys -> return (x :: ys))

(*
append_tr [1;2;3] [4;5;6] (fun x -> x) ;; --> set return as the identity function
- : int list = [1; 2; 3; 4; 5; 6]
*)


let rec product (l: int list) : int = 
  match l with 
  | [] -> 1
  | x :: xs -> 
      if x = 0 then 0 
      else x * product xs
                 
                 
let rec product_tr (l: int list) (return_outside : int -> 'r) : 'r = 
  
  let rec product_tr' (l: int list) (return : int -> 'r) : 'r = 
    match l with 
    | [] -> return 1
    | x :: xs -> 
        if x = 0 then return_outside 0 (* kind of a break statement *)
        else product_tr' xs (fun n -> return (x * n))
  
  in product_tr' l return_outside
  







