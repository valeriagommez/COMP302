(** Substitution & Evaluation *)
let free_vars_test_helper_tests : (exp * ident list) list = [
  (ConstI 5, []);
  (Var "x", ["x"])
]

let rec free_vars (e : exp) : IdentSet.t =
  match e with
  | ConstI _ -> IdentSet.empty
  | PrimBop (e1, _, e2) -> IdentSet.union (free_vars e1) (free_vars e2)
  | PrimUop (_, e') -> free_vars e'

  | ConstB _ -> IdentSet.empty
  | If (e', e1, e2) -> (* implement this *)
      let freeVars12 = IdentSet.union (free_vars e1) (free_vars e2) 
      in IdentSet.union (free_vars e') freeVars12 

  | Comma (e1, e2) -> IdentSet.union (free_vars e1) (free_vars e2)
  | LetComma (x, y, e1, e2) -> (* implement this *)
      let freeVars2 = IdentSet.remove x (IdentSet.remove y (free_vars e2))
      in IdentSet.union (free_vars e1) freeVars2
        (* x and y are bound in e2, NOT in e1 *) 

  | Fn (x, tOpt, e') -> (* implement this *)
      IdentSet.remove x (free_vars e') 
                        
  | Apply (e1, e2) -> (* implement this *)
      IdentSet.union (free_vars e1) (free_vars e2) 
                      
  | Rec (f, tOpt, e') -> (* implement this *)
      IdentSet.remove f (free_vars e')     

  | Let (x, e1, e2) -> (* implement this *)
      IdentSet.union (free_vars e1) (IdentSet.remove x (free_vars e2)) 
                       
  | Var x -> IdentSet.singleton x

(** DO NOT Change This Definition *)
let free_vars_test_helper e = IdentSet.elements (free_vars e)

let subst_tests : (((exp * ident) * exp) * exp) list = [
  (((ConstI 5, "x"), PrimBop (ConstI 2, Plus, Var "x")), PrimBop (ConstI 2, Plus, ConstI 5))
]

let rec subst ((d, z) : exp * ident) (e : exp) : exp =
  (** [rename (x, e)] replace [x] appears in [e] with a fresh identifier
      and returns the fresh identifier and updated expression *)
  let rename ((x, e) : ident * exp) : ident * exp =
    let x' = fresh_ident x in
    (x', subst (Var x', x) e)
  in
  match e with
  | ConstI _ -> e
  | PrimBop (e1, bop, e2) -> PrimBop (subst (d, z) e1, bop, subst (d, z) e2)
  | PrimUop (uop, e') -> PrimUop (uop, subst (d, z) e')

  | ConstB _ -> e
  | If (e', e1, e2) -> (* implement this *)
      If (subst (d, z) e', subst (d, z) e1, subst (d, z) e2)

  | Comma (e1, e2) -> Comma (subst (d, z) e1, subst (d, z) e2)
  | LetComma (x, y, e1, e2) -> (* implement this *)
      let freeVars = free_vars d in
      let (x', e2) = 
        if x = z || IdentSet.mem x freeVars then rename (x, e2) else (x, e2)
      in
      let (y', e2) = 
        if y = z || IdentSet.mem y freeVars then rename (y, e2) else (y, e2)
      in LetComma (x', y', subst (d, z) e1, subst (d, z) e2)
        
  | Fn (x, tOpt, e') -> (* implement this *)
      let freeVars = free_vars d in
      let (x', e') = 
        if x = z || IdentSet.mem x freeVars then rename (x, e') else (x, e')
      in
      Fn (x', tOpt, subst (d, z) e')
                        
  | Apply (e1, e2) -> (* implement this *)
      Apply (subst (d, z) e1, subst (d, z) e2)

  | Rec (f, tOpt, e') -> (* implement this *)
      let freeVars = free_vars d in
      let (f', e') = 
        if f = z || IdentSet.mem f freeVars then rename (f, e') else (f, e')
      in 
      Rec (f', tOpt, subst (d, z) e')

  | Let (x, e1, e2) -> (* implement this *)
      let freeVars = free_vars d in
      let (x', e2) = 
        if x = z || IdentSet.mem x freeVars then rename (x, e2) else (x, e2)
      in
      Let (x', subst (d, z) e1, subst (d, z) e2)
             
  | Var x ->
      if x = z
      then d
      else e

let eval_test_helper_tests : (exp * exp option) list = [
  (Var "x", None);
  (ConstI 5, Some (ConstI 5));
  (PrimBop (ConstI 5, Minus, ConstI 5), Some (ConstI 0))
]

let rec eval (e : exp) : exp =
  match e with
  | ConstI _ -> e
  | PrimBop (e1, bop, e2) ->
      begin
        match eval e1, eval e2 with
        | ConstI n1, ConstI n2 ->
            begin
              match bop with
              | Equals -> ConstB (n1 = n2)
              | LessThan -> ConstB (n1 < n2)
              | Plus -> ConstI (n1 + n2)
              | Minus -> ConstI (n1 - n2)
              | Times -> ConstI (n1 * n2)
            end
        | _ -> raise EvaluationStuck
      end
  | PrimUop (_, e) ->
      begin
        match eval e with
        | ConstI n -> ConstI (- n)
        | _ -> raise EvaluationStuck
      end

  | ConstB _ -> e
  | If (e', e1, e2) -> (* implement this *)
      begin
        match eval e' with
        | ConstB true -> eval e1
        | ConstB false -> eval e2
        | _ -> raise EvaluationStuck
      end 

  | Comma (e1, e2) -> Comma (eval e1, eval e2)
  | LetComma (x, y, e1, e2) -> (* implement this *)
      begin
        match eval e1 with 
        | Comma (x', y') -> eval (subst (x', x) (subst (y', y) e2))
        | _ -> raise EvaluationStuck
      end 

  | Fn (x, tOpt, e') -> (* implement this *)
      e
                        
  | Apply (e1, e2) -> (* implement this *)
      begin
        let results2 = eval e2 in
        let results1 = eval e1 in
        match results1 with 
        | Fn (x, tOpt, e') -> eval (subst (results2, x) e')
        | _ -> raise EvaluationStuck
      end

  | Rec (f, tOpt, e') -> (* implement this *)
      eval (subst (e, f) e')

  | Let (x, e1, e2) -> (* implement this *)
      eval (subst (eval e1, x) e2)
                       
  | Var _ -> (* implement this *)
      raise EvaluationStuck
             

(** DO NOT Change This Definition *)
let eval_test_helper e =
  try
    Some (eval e)
  with
  | EvaluationStuck -> None
