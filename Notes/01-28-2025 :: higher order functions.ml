(* Higher order function *)

let rec map (f : 'a -> 'b) (l : 'a list) : 'b list  = 
  match l with 
  | [] -> []
  | x :: xs -> f x :: map f xs
                 
  
(* 

   map (fun x -> x * 2) [1;2;3] ;;
- : int list = [2; 4; 6]

*)

let rec filter (p : 'a -> bool) (l : 'a list) : 'a list  = 
  match l with 
  | [] -> []
  | x :: xs -> 
      if p x then x :: filter p xs 
      else filter p xs
          
   (* if x is good => we keep it and move on *)

(* 

   filter (fun x -> x mod 2 = 0) [1;2;3] ;;
- : int list = [2]

*)


(* 
Use a combo of map and filter to substract 5 from every element in a list
of ints and then pickl only those that are greater than 10 
*)

let add_5_etc (l : 'a list) : 'b list  = 
  filter (fun x -> x > 10) (map (fun x -> x - 5) l)
