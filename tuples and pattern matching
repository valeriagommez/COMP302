(* Doing this using pattern matching w/ tuples *)

let is_tall p = snd p > 178
                
let is_tall' p = 
  let (name, height) = p in height > 178
  
  
(* 
   is_tall ("jake", 182)
   ---> let (name, height) = ("jake" 182) in height > 178 
   ---> 182 > 178 ---> True

- we can think of the variables as place holders 
- since we never use "name", we don't need to initialize a variable name for it. 
  Instead, we can put an underscore '_'
   - let (_, height) = p in height > 178 
*)

