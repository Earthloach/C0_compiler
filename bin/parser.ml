open Ast

type token_stream = 
  { lex : Lexer.lexer
  ; mutable la : Token.t option
  }

(* Parser utils *)
let peek s = 
  match s.la with 
  | Some tok -> tok
  | None -> 
      let tok = Lexer.next_token s.lex in
      s.la <- Some tok;
      tok

let take s =
  let tok = peek s in
  s.la <- None;
  tok

let expect s expected = 
  let got = take s in
  if not (Token.equal got expected) then 
    Fmt.failwith "expect: expected %s, got %s" (Token.show expected) (Token.show got)

(* Parser functions*)
let parse_exp (s : token_stream) : exp = 
  match take s with 
  | Token.Dec_const i -> Const i
  | Token.Hex_const i -> Const i

let parse_simp (s : token_stream) (name : Symbol.t): simp = 
  let op = match take s with 
  | Token.Assign -> Eq
  | Token.PlusEq -> PlusEq
  | Token.MinusEq -> MinusEq
  | Token.StarEq -> TimesEq 
  | Token.SlashEq -> DivideEq
  | Token.PercentEq -> ModuloEq
  | tok -> Fmt.failwith "invalid assign operator" (Token.show tok)
  in 
  let value = parse_exp s in 
  Asnop {op; name; value}

let parse_decl s = 
  match take s with
  | Token.Ident (sym) -> if Token.equal (peek s) (Token.Assign) then
                         let _ = take s in 
                         Init (sym, parse_exp s)
                         else New_var sym
  | tok -> Fmt.failwith "invalid declaration token: %s" (Token.show tok)

let parse_stm s =
  match take s with 
  | Token.Return -> 
      let e = parse_exp s in 
      expect s Token.Semicolon;
      Return e
  | Token.Ident (sym) -> 
      let simp = parse_simp s sym in 
      expect s Token.Semicolon;
      Assign simp
  | Token.Int -> 
      let decl = parse_decl s in
      expect s Token.Semicolon;
      Delcare decl
  | tok -> Fmt.failwith "invalid statement token: %s" (Token.show tok)



  


