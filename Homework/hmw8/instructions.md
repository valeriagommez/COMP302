A DIY programming language: MiniCAML

Note: the prelude for this exercise is quite long. However, we will point you to the useful functions in the prelude when relevant, so you shouldn’t try to read it all at once.

Part 1: Parsing

The starting point of processing a language is transforming a sequence of characters into an abstract syntax tree (AST). Programmers write a sequence of characters in their editors and through parsing a compiler turns the sequence of characters into a meaningful structure, the Abstract Syntax Tree, that can be used in later phases of the compiler. A parser takes a sequence of characters, then consumes it (usually from first-to-last, one by one) to analyze what that characters mean.

In this assignment, you will implement a parser for a language called MiniCaml using parser combinators like you saw in class. Parser combinators compose a parser by combining parser primitives. In the prelude, we provide the module Parser that contains the primitives. The type of parser in that module is Parser.t.

The most basic primitives are run, of_value, (|*>), fail, and first_of_2.

run p s executes the parser p with the input string s. If p succeeds, this function returns Some <result of p>. If p fails, this returns None.
of_value v creates a parser that does not consume any input, but gives the result v.
p |*> f creates a parser that goes through p first, and then use its result to parse further. For example, p |*> fun x -> of_value (3 * x) multiplies 3 to the parsing result of p.
fail is a parser that always fails without consuming any input.
first_of_2 p q first tries p, and if it fails, this restores the input to the state before p and tries q. This allows us to define a parser with multiple cases. This fails if both p and q fail.
As described, these do not consume any input, but we also need primitives that actually consume input. accept_char, accept_string, satisfy, and satisfy_many are such basic primitives.

accept_char c consumes a character c if it is the starting character of the leftover input. Otherwise this parser fails.
accept_string s consumes a string s if it is the prefix of the leftover input. Otherwise this parser fails.
satisfy p consumes a starting character of the leftover input if it satisfies the predicate p. Otherwise this parser fails.
satisfy_many p consumes the all consecutive characters of the leftover input satisfying the predicate p. This parser never fails. If there is no prefix that satisfies the predicate, the result is "".
We build a parser by combining these primitives. The combinators allowing us to combine them in common ways are (|>>), map, map2, map3, const_map, first_of, many, some, sep_by, sep_by_1, optional, skip, no, and between.

p |>> q goes through p first, and then q. Its result is the result of q. The result of p is ignored. This fails if either p or q fails.
map f p applies the function f to the result of p.
map2 f p q applies the function f to the both results of p and q together. Note that p will consume the input first, and then q will consume the leftover input after p. This fails if either p or q fails.
map3 f p q r applies the function f to the result of p, q, and r. Note that p will consume the input first, and then q will consume the leftover input after p. This fails if one of p, q, or r fails.
const_map v p replaces the result of p with v while otherwise behaving in the same way as p.
first_of ps tries parsers in ps in order. This fails if all ps fail.
many p tries p as many times as it succeeds. This never fails. If p fails for its first try, the result is [].
some p tries p as many times as it succeeds, but it should succeed at least once. This fails if p fails for its first try.
sep_by p sep tries to parse instances of p, separated by instances of sep, as many times as possible. For example, sep_by (accept_char 'a') (accept_char 'b') will accept "", "a","aba", "ababa", etc.
sep_by_1 p sep is to sep_by like some is to many. It parses instances of p separated by instances of sep, but insists that there be at least one instance of p.
optional p tries p. If it succeeds, the result is Some <result of p>. If p fails, this restores the input to the state before p, and its result is None.
skip p replaces the result of p with (), i.e. skipping the result of p. This is useful for something like between, which only takes unit Parser.t.
no p succeeds only if p fails.
between p q r goes through p, and then r, and finally q. For example, between (skip (accept_char '(')) (skip (accept_char ')')) (accept_char '5') parses "(5)". Its result is the result of r.
With these combinators, we can build high-level helpers like eof, spaces, lexeme, symbol, keyword_among, identifier_except, digits, int_digits, optional_prefix_op, right_assoc_op, left_assoc_op, and non_assoc_op

eof succeeds if and only if the input has no more characters.
spaces consumes all consecutive spaces from the head of the leftover input. This never fails, even when there is no spaces at the head.
lexeme p behaves the same as p except that this consumes spaces after what p consumed.
symbol s consumes the string s and spaces after that, and gives () as its result. This is meant to be used for non-alphanumeric characters, such as "->" or "+".
keyword_among kwds s consumes the string s (and spaces after that) as a keyword; there should be no alphanumeric characters immediately following it, and s should be in kwds, the keyword list.
identifier_except kwds s consumes any alphanumeric string (and spaces after that) starting with an alphabet letter except the ones specified in kwds. If the result is in kwds, this parser fails.
digits consumes "0" or any decimal digits starting with a non-zero decimal digit.
int_digits is digits except that this converts its result into OCaml int instead of string.
prefix_op operatorp operandp con parses operatorp as a optional prefix of operandp, and combines their results using the function con.
right_assoc_op operatorp operandp con parses operatorp as an optional right associative binary operator for operandps, and combines their results using the function con. For example, when we think ^ as a right associative operator, Paring "5 ^ 4 ^ 3" with right_assoc_op (symbol "^") int_digit con gives con 5 () (con 4 () 3). Here, the second argument to con is the result of operatorp (e.g. the result () of symbol "^").
left_assoc_op operatorp operandp con parses operatorp as an optional left associative binary operator for operandps, and combines their results using the function con. For example, when we think - as a left associative operator, Paring "5 - 4 - 3" with left_assoc_op (symbol "-") int_digit con gives con (con 5 () 4) () 3. Here, the second argument to con is the result of operatorp.
non_assoc_op operatorp operandp con parses operatorp as an optional non-associative binary operator for two operandp, and combines their results using the function con. Here, the second argument to con is the result of operatorp.
The grammar of MiniCAML as follows.

atomic_typ = "int"        (* Integer type *)
           | "bool"       (* Boolean type *)
           | "(" type ")" (* Parenthesized type *)

pair_typ = atomic_typ "*" atomic_typ (* Pair type *)
         | atomic_typ

typ = pair_typ "->" typ (* Arrow type, i.e. function type *)
    | pair_typ

atomic_exp = "true"      (* Boolean constant true *)
           | "false"     (* Boolean constant false *)
           | digits      (* Integer constants *)
           | ident       (* Variables *)
           | "(" exp ")" (* Parenthesized expression *)

applicative_exp := applicative_exp " " atomic_exp (* Left associative function application *)
                 | atomic_exp

negatable_exp = "let" "(" ident "," ident ")" "=" exp "in" exp "end"  (* Let comma binding *)
              | "let" ident "=" exp "in" exp "end"                    (* Let binding *)
              | "if" exp "then" exp "else" exp                        (* If-then-else expression *)
              | "fn" ident [":" typ] "=>" exp                         (* Function with optional type annotation *)
              | "rec" ident [":" typ] "=>" exp                        (* Recursion with optional type annotation *)
              | applicative_exp

negation_exp = ["-"] negatable_exp (* Prefix unary operation - *)

multiplicative_exp = multiplicative_exp "*" negation_exp (* Left associative binary op * *)
                   | negation_exp

additive_exp = additive_exp "+" multiplicative_exp (* Left associative binary op + *)
             | additive_exp "-" multiplicative_exp (* Left associative binary op - *)
             | multiplicative_exp

comparative_exp = additive_exp "=" additive_exp (* Non-associative binary op = *)
                | additive_exp "<" additive_exp (* Non-associative binary op < *)
                | additive_exp

exp = comparative_exp "," comparative_exp (* Non-associative binary operation pair construction (comma) *)
    | comparative_exp
Here, [] means that anything inside the square brackets is optional, e.g. both fn x => x and fn x : int => x are valid for the grammar "fn" ident [":" typ] "=>" exp.

We show the grammar for typ for completeness, but note that the parser typ_parser for typ is already provided in the prelude. Your task is to implement the parser exp_parser_impl. For this, you may need to add helper parsers for each grammar class of expressions.

Note: you may see an error like This kind of expression is not allowed as right-hand side of 'let rec'. This is due to OCaml’s recursion checker. This can happen if you use exp_parser_impl in a recursive call instead of exp_parser. In that case, please replace exp_parser_impl calls with exp_parser calls.

Hint: if your parser does not terminate, It is quite likely due to how you parse additive_exp or multiplicative_exp. Parser.left_assoc_op may help you to fix that.
