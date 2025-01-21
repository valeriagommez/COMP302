(* Polymorphism *)

let id x = x (* 'a --> 'a *)

let const x y = x (* 'a --> 'b --> 'a *)
                  
let const z y = 
  let _ = y + 1 in
  z (* 'a --> int --> 'a 
even if we're not using y in any way, we're still making it an 'int' 
since we're performing an operation on it *)
    
