(* normal function *)

let rec factorial (n : int) : int =
  if n = 0 then 1 else n * factorial (n - 1) 
                                             

(* using TCO *)

let rec factorial' (n : int) (curAnswer : int) : int = 
  if n = 0 then curAnswer else factorial' (n - 1) (curAnswer * n) 
      
      
(* other method *)

let factorial'' n = 
  let rec fact_tr (acc : int) (n : int) : int = 
    if n = 0 then acc else fact_tr (acc * n) (n-1)
  in
  fact_tr 1 n
