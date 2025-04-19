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
    left_assoc_op 
      (symbol " ") 
      atomic_exp 
      (fun f () x -> Apply(f, x))
  in
      
  let negatable_exp = 
    first_of 
      [
        (* LetComma of ident * ident * exp * exp  *) 
        map3 
          (fun (x, y) e1 e2 -> LetComma(x, y, e1, e2))
          (keyword "let" |>> 
           between 
             (skip (accept_char '(')) 
             (skip (accept_char ')'))
             (map2 
                (fun x y -> (x,y)) 
                (identifier) 
                (skip (symbol "(") |>> identifier)
             )
          )
          (symbol "=" |>> exp_parser)
          (between (keyword "in") (keyword "end" ) exp_parser) ; 
        
        
        (*  Let of ident * exp * exp  *)
        map3 
          (fun x e1 e2 -> Let(x, e1, e2))
          (keyword "let" |>> identifier)
          (symbol "=" |>> exp_parser)
          (between (keyword "in") (keyword "end" ) exp_parser);
        
        (* if e then e1 else e2 *)
        map3
          (fun e e1 e2 -> If(e, e1, e2))
          (keyword "if" |>> exp_parser)
          (keyword "then" |>> exp_parser)
          (keyword "else" |>> exp_parser) ; 
        
        (* fn x : t => e  OR  fn x => e *) 
        map3 
          (fun x t e -> Fn(x, t, e))
          (keyword "fn" |>> identifier)
          (optional (keyword ":" |>> typ_parser))
          (symbol "=>" |>> exp_parser) ;
        
        (* rec f : t => e  OR  rec f => e *)
        map3 
          (fun x t e -> Rec(x, t, e))
          (keyword "rec" |>> identifier)
          (optional (keyword ":" |>> typ_parser))
          (symbol "=>" |>> exp_parser) ;
        
        (* else *)
        applicative_exp 
      ]
  in
  
  let negation_exp = 
    prefix_op 
      (symbol "-")
      (negatable_exp)
      (fun () x -> PrimUop (Negate, x)) 
  in
  
  let multiplicative_exp = 
    left_assoc_op 
      (symbol "*") 
      (negation_exp) 
      (fun e1 () e2 -> PrimBop(e1, Times, e2));
  in
  
  let additive_exp = 
    left_assoc_op 
      (first_of [
          const_map Minus (symbol "-") ;
          const_map Plus (symbol "+") 
        ]
      ) 
      (multiplicative_exp) 
      (fun e1 operator e2 -> PrimBop(e1, operator, e2));
    
  in
  
  let comparative_exp = 
    left_assoc_op 
      (first_of [
          const_map Equals (symbol "<") ;
          const_map LessThan (symbol "=") 
        ]
      ) 
      (additive_exp) 
      (fun e1 operator e2 -> PrimBop(e1, operator, e2));
  in 
  
  let exp_parser_impl = 
    non_assoc_op
      (symbol ",")
      comparative_exp
      (fun e1 _ e2 -> Comma(e1, e2)) 
    (* END OF IMPLEMENTATION *)
  in
  exp_parser_impl i

(** DO NOT Change This Definition *)
let parse_exp : string -> exp option =
  let open Parser in
  run (between spaces eof exp_parser)
