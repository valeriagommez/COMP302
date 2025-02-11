let product_tr l return_outside =
  let rec go l return =
    match l with
    | [] -> return 1 
    | x :: xs ->
        if x = 0 then return_outside 0
        else go xs (fun n -> return (n*x) ) 
  in go l return_outside
    
    
exception SawZero (* capital letter ==> it's a constructor! *)
                  
let product_tr l = 
  let rec go l = 
    match l with 
    | [] -> 1
    | x :: xs ->
        if x = 0 then raise SawZeroo
        else x * go xs
  in go l
            
            
(* raise : exn (exception) -> 'a 
    - it returns ANYTGHING, however, it doesn't actually return ever 
*)
