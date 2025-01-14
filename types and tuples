(* Creating types and tuples *)

type height = int
type name = string
  
let person = height * name (* <- TUPLE!! *)
                           
let make_person (h : height) (n : name) : person = (h, n)
  
let grow (p : person) (h : height) : person = 
  make_person (fst p + h) (snd p)
    
let grow' (p : person) (h : height) : person = 
  let (old_height, name) = p in 
  make_person (old_height + h) name 
