(** Part 1: Parsing *)
let parse_exp_tests : (string * exp option) list = [
  ("", None)
]

let rec exp_parser i =
  let open Parser in
  (** Use [identifier] and [keyword] in your implementation,
      not [identifier_except] and [keyword_among] directly *)
  let identifier, keyword =
    let keywords = ["true"; "false"; "let"; "in"; "end"; "if"; "then"; "else"; "fn"; "rec"] in
    identifier_except keywords, keyword_among keywords
  in
  (* You may need to define helper parsers depending on [exp_parser] here *)
  
  (* IMPLEMENT HERE : *)
  
  let atomic_exp = 
    (* ALWAYS USE MAP!! *) 
    first_of 
      [
        const_map (ConstB true) (keyword "true");
        const_map (ConstB false) (keyword "false");
        map (fun n -> ConstI n) (int_digits);
        map (fun x -> Var x) (identifier);
        between (symbol "(") (symbol ")") (exp_parser)
      ]
  in
  
  let applicative_exp = (* applying parameter to function *)
                            
    (* 
    
    first_of 
      [
        map2 (fun f x -> Apply(f, x)) 
          applicative_exp 
          (symbol " " |*> atomic_exp);
        
        atomic_exp
      ]
  
    *)
                            
    left_assoc_op (symbol " ") atomic_exp (fun f () x -> Apply(f, x))
  in
      
  let negatable_exp = 
    first_of 
      [
        (* LetComma of ident * ident * exp * exp  *)
        (* 
           map3 (
            (fun (x, y) e1 e2 -> LetComma(x, y, e1, e2))
              (keyword "let" |*> typ_parser)
              (symbol "=" |*> exp_parser)
              (keyword "in" |*> exp_parser <* keyword "end")
          );
        *)
        
        (*  Let of ident * exp * exp  *)
        map3 (
          (fun x e1 e2 -> Let(x, e1, e2))
            (keyword "let" |>> map (fun x -> Var x) (identifier))
            (symbol "=" |*> exp_parser)
            (keyword "in" |*> exp_parser |*> keyword "end")
        ); 
        
        (* if e then e1 else e2 *)
        map3 (
          (fun e e1 e2 -> If(e, e1, e2))
            (keyword "if" |*> exp_parser)
            (keyword "then" |*> exp_parser)
            (keyword "else" |*> exp_parser)
        ); 
        
        (* fn x : t => e  OR  fn x => e *) 
        map3 (
          (fun x t e -> Fn(x, t, e))
            (keyword "fn" |*> map Var ident)
            (optional (keyword ":" |*> typ_parser))
            (symbol "=>" |*> exp_parser)
        );
        
        (* rec f : t => e  OR  rec f => e *)
        map3 (
          (fun x t e -> Rec(x, t, e))
            (keyword "rec" |*> map Var ident)
            (optional (keyword ":" |*> typ_parser))
            (symbol "=>" |*> exp_parser)
        );
        
        (* else *)
        applicative_exp 
      ]
  in
  
  let negation_exp = 
    map( 
      (fun x -> PrimUop (Negate, x))
        optional (symbol "-")
    )
  in
  
  let rec multiplicative_exp = 
    first_of 
      [
        (* multiplicative_exp "*" negation_exp *)
        map2 (
          (fun e1 e2 -> PrimBop(e1, Times, e2))
            multiplicative_exp
            (symbol "*" |*> negation_exp)
            
        );
        
        negation_exp
      ]
  in
  
  let rec additive_exp = 
    first_of 
      [
        (* additive_exp "+" multiplicative_exp *)
        map2 (
          (fun e1 e2 -> PrimBop(e1, Plus, e2))
            (additive_exp)
            (symbol "+" |*> multiplicative_exp)
        );
        
        (* additive_exp "-" multiplicative_exp *) 
        map2 (
          (fun e1 e2 -> PrimBop(e1, Minus, e2))
            (additive_exp)
            (symbol "-" |*> multiplicative_exp)
        );
        
        multiplicative_exp 
      ] 
  in
  
  let comparative_exp =
    first_of 
      [
        (* additive_exp "=" additive_exp *)
        map2 (
          (fun e1 e2 -> PrimBop(e1, Equals, e2))
            (additive_exp)
            (symbol "=" |*> additive_exp)
        );
        
        (* additive_exp "<" additive_exp *)
        map2 (
          (fun e1 e2 -> PrimBop(e1, LessThan, e2))
            (additive_exp)
            (symbol "<" |*> additive_exp)
        );
        
        additive_exp
      ] 
  in
  
  let exp = 
    first_of 
      [
        (* comparative_exp "," comparative_exp *)
        map2 (
          (fun e1 e2 -> Comma(e1, e2))
            (comparative_exp)
            (symbol "," |*> comparative_exp)
        );
        
        comparative_exp
      ]
  in
  
  let exp_parser_impl = 
    first_of 
      [
        atomic_exp ;
        applicative_exp;
        negatable_exp;
        negation_exp;
        multiplicative_exp;
        additive_exp;
        comparative_exp;
        exp
      ]
    (* END OF IMPLEMENTATION *)
  in
  exp_parser_impl i

(** DO NOT Change This Definition *)
let parse_exp : string -> exp option =
  let open Parser in
  run (between spaces eof exp_parser)
