(** Part 3: Type Inference *)
let typ_infer_test_helper_tests : ((Context.t * exp) * typ option) list = [
  ((Context.empty, ConstB true), Some Bool)
]

let rec typ_infer (ctx : Context.t) (e : exp) : typ =
  match e with
  | ConstI _ -> Int
  | PrimBop (e1, bop, e2) ->
      let ((t1, t2), t3) = bop_type bop in
      if typ_infer ctx e1 = t1 && typ_infer ctx e2 = t2
      then t3
      else raise TypeInferenceError
  | PrimUop (uop, e') ->
      let (t1, t2) = uop_type uop in
      if typ_infer ctx e' = t1
      then t2
      else raise TypeInferenceError

  | ConstB _ -> Bool
  | If (e', e1, e2) -> (* implement this! *)
      begin
        let type1 = typ_infer ctx e1 in
        let type2 = typ_infer ctx e2 in 
        match typ_infer ctx e' with 
        | Bool -> 
            if type1 == type2 then type1
            else raise TypeInferenceError 
        | _ -> raise TypeInferenceError
      end

  | Comma (e1, e2) -> Pair (typ_infer ctx e1, typ_infer ctx e2)
                        
  | LetComma (x, y, e1, e2) -> (* implement this! *)
      begin
        match typ_infer ctx e1 with 
        | Pair (a, b) -> 
            let ctx1 = (Context.extend ctx (x, a)) in 
            let ctx2 = (Context.extend ctx1 (y, b)) in 
            typ_infer ctx2 e2
        | _ -> raise TypeInferenceError 
      end 
      
  | Fn (x, Some t, e') -> (* implement this! *) 
      let outputType = typ_infer (Context.extend ctx (x, t)) e' in
      Arrow (t, outputType) 

  | Apply (e1, e2) -> (* implement this! *)
      begin
        match typ_infer ctx e1 with 
        | Arrow (t1, t2) -> 
            let t2' = typ_infer ctx e2 in
            if t2 = t2' then t2
            else raise TypeInferenceError
        | _ -> raise TypeInferenceError
      end

  | Rec (f, Some t, e') -> (* implement this! *)
      typ_infer ctx e'
        
  | Let (x, e1, e2) -> (* implement this! *)
      let type1 = typ_infer ctx e1 in
      typ_infer (Context.extend ctx (x, type1)) e2
                       
  | Var x ->
      begin
        match Context.lookup ctx x with
        | Some t -> t
        | None -> raise TypeInferenceError
      end

  (** You can ignore these cases for Part 2 *)
  | Fn (_, None, _) -> raise IgnoredInPart3
  | Rec (_, None, _) -> raise IgnoredInPart3

(** DO NOT Change This Definition *)
let typ_infer_test_helper ctx e =
  try
    Some (typ_infer ctx e)
  with
  | TypeInferenceError -> None

(** Part 4: Unification & Advanced Type Inference *)
let unify_test_case1 () =
  let x = new_tvar () in
  let y = new_tvar () in
  y := Some Int;
  (TVar x, TVar y)

let unify_test_case2 () =
  let x = new_tvar () in
  (TVar x, Arrow (TVar x, TVar x))

let unify_test_helper_tests : ((unit -> typ * typ) * bool) list = [
  ((fun () -> (Int, Int)), true);
  ((fun () -> (Int, Bool)), false);
  (unify_test_case1, true);
  (unify_test_case2, false)
]

let rec unify : typ -> typ -> unit =
  let rec occurs_check (x : typ option ref) (t : typ) : bool =
    let t = rec_follow_tvar t in
    match t with
    | Int -> false
    | Bool -> false
    | Pair (t1, t2) -> (* implement this! *) 
        occurs_check x t1 || occurs_check x t2
          
    | Arrow (t1, t2) -> (* implement this! *)
        occurs_check x t1 || occurs_check x t2   
          
    | TVar y -> (* implement this! *)
        is_same_tvar x y
  in
  fun ta tb ->
    let ta = rec_follow_tvar ta in
    let tb = rec_follow_tvar tb in
    match ta, tb with
    | Int, Int -> ()
    | Bool, Bool -> ()
                    
    | Pair (ta1, ta2), Pair (tb1, tb2) -> (* implement this! *)
        if (unify ta1 ta2) == () && (unify tb1 tb2) == () then ()
        else raise UnificationFailure
                                          
    | Arrow (ta1, ta2), Arrow (tb1, tb2) -> (* implement this! *)
        if (unify ta1 ta2) == () && (unify tb1 tb2) == () then ()
        else raise UnificationFailure
                                            
    | TVar xa, TVar xb when is_same_tvar xa xb -> ()
                                                  
    | TVar xa, _ -> (* implement this! *)
        if occurs_check xa tb then raise OccursCheckFailure 
        else unify ta tb
                    
    | _, TVar xb -> unify tb ta
    | _, _ -> raise UnificationFailure

(** DO NOT Change This Definition *)
let unify_test_helper f =
  let ta, tb = f () in
  try
    unify ta tb; true
  with
  | UnificationFailure -> false
  | OccursCheckFailure -> false

let adv_typ_infer_test_case1 =
  let x = new_tvar () in
  ((Context.empty, Fn ("y", None, Var "y")), Some (Arrow (TVar x, TVar x)))

let adv_typ_infer_test_helper_tests : ((Context.t * exp) * typ option) list = [
  adv_typ_infer_test_case1
]

let rec adv_typ_infer (ctx : Context.t) (e : exp) : typ =
  match e with
  | ConstI n -> (* implement this! *)
      Int
  | PrimBop (e1, bop, e2) -> (* implement this! *)
      let ((t1, t2), t3) = bop_type bop in
      let t1b = adv_typ_infer ctx e1 in 
      let t2b = adv_typ_infer ctx e2 in
      unify t1 t1b ; 
      unify t2 t2b ; 
      t3
                             
  | PrimUop (uop, e') -> (* implement this! *)
      let (t1, t2) = uop_type uop in
      let t1b = adv_typ_infer ctx e' in
      unify t1 t1b ;
      t2 

  | ConstB b -> (* implement this! *)
      Bool
                
  | If (e', e1, e2) -> (* implement this! *)
      begin
        let t1 = adv_typ_infer ctx e1 in 
        let t2 = adv_typ_infer ctx e2 in 
        let t = adv_typ_infer ctx e' in
        unify t Bool;
        unify t1 t2 ;
        t2
      end 
      
  | Comma (e1, e2) -> (* implement this! *)
      begin
        let t1 = adv_typ_infer ctx e1 in 
        let t2 = adv_typ_infer ctx e2 in 
        Pair (t1, t2)
      end 
                      
  | LetComma (x, y, e1, e2) -> (* implement this! *)
      begin
        match adv_typ_infer ctx e1 with 
        | Pair (a, b) -> 
            let ctx1 = (Context.extend ctx (x, a)) in
            let ctx2 = (Context.extend ctx1 (y, b)) in
            adv_typ_infer ctx2 e2 
        | _ -> raise TypeInferenceError
      end 

  | Fn (x, Some t, e') -> (* implement this! *)
      let ctx1 = (Context.extend ctx (x, t)) in
      let outputType = adv_typ_infer ctx1 e' in
      Arrow (t, outputType)
                          
  | Fn (x, None, e') -> 
      let typeX = TVar (ref None) in 
      let ctx1 = (Context.extend ctx (x, typeX)) in
      let outputType = adv_typ_infer ctx1 e' in 
      Arrow (typeX, outputType) 
        
  | Apply (e1, e2) -> (* implement this! *) 
      begin
        match adv_typ_infer ctx e1 with 
        | Arrow (t1, t2) -> 
            let t2' = adv_typ_infer ctx e2 in
            unify t2 t1;
            t2
        | _ -> raise TypeInferenceError
      end

  | Rec (f, Some t, e') -> (* implement this! *)
      let ctx1 = (Context.extend ctx (f, t)) in
      let outputType = adv_typ_infer ctx1 e' in
      unify outputType t ; 
      t
                           
  | Rec (f, None, e') -> 
      let value = TVar (ref None) in
      let ctx1 = (Context.extend ctx (f, value)) in
      let outputType = adv_typ_infer ctx1 e' in
      unify outputType value ; 
      value

  | Let (x, e1, e2) -> (* implement this! *)
      let t1 = adv_typ_infer ctx e1 in
      adv_typ_infer(Context.extend ctx (x, t1)) e2
  
  | Var x -> (* implement this! *)
      begin
        match Context.lookup ctx x with
        | Some t -> t
        | None -> raise TypeInferenceError
      end

(** DO NOT Change This Definition *)
let adv_typ_infer_test_helper ctx e =
  try
    Some (adv_typ_infer ctx e)
  with
  | UnificationFailure -> None
  | OccursCheckFailure -> None
  | TypeInferenceError -> None

(**
 ************************************************************
 You Don't Need to Modify Anything After This Line
 ************************************************************

 Following definitions are the helper entrypoints
 so that you can do some experiments in the top-level.
 Once you implement [typ_infer] you can test it with
 [infer_main] in the top-level.
 Likewise, once you implement [adv_typ_infer] you can
 test it with [adv_infer_main] in the top-level.
 *)
let infer_main exp_str =
  match parse_exp exp_str with
  | None -> raise ParserFailure
  | Some e ->
      print_string "input expression       : "; print_exp e; print_newline ();
      let t = typ_infer Context.empty e in
      print_string "type of the expression : "; print_typ t; print_newline ();
      print_string "evaluation result      : "; print_exp (eval e); print_newline ()

let adv_infer_main exp_str =
  match parse_exp exp_str with
  | None -> raise ParserFailure
  | Some e ->
      print_string "input expression       : "; print_exp e; print_newline ();
      let t = adv_typ_infer Context.empty e in
      print_string "type of the expression : "; print_typ t; print_newline ();
      print_string "evaluation result      : "; print_exp (eval e); print_newline ()
