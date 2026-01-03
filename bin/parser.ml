open Ast

type token_stream = 
  { lex : Lexer.lexer
  ; mutable la : Token.t option
  }

let init_token_stream cotent =
  let lexer = Lexer.init_lexer cotent in 
  {lex = lexer; la = None}

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


let precedence tok = 
  let open Token in 
  match tok with
    | Star | Slash | Percent -> 50
    | Plus | Minus -> 30
    | _ -> -1  (* non-binop *)

  
(* Parser functions*)
let parse_unop s = 
  match take s with
  | Token.Minus -> Negative
  | tok -> Fmt.failwith "invalid unop token %s" (Token.show tok)

let parse_binop s = 
  match take s with 
  | Token.Plus -> Add
  | Token.Minus -> Sub
  | Token.Star -> Mul
  | Token.Percent -> Mod
  | tok -> Fmt.failwith "invalid binop tok %s" (Token.show tok)

let rec parse_factor s = 
  match peek s with 
  | Token.Dec_const c -> 
      ignore (take s); 
      Const c
  | Token.Hex_const c -> 
      ignore (take s);
      Const c 
  | Token.Minus -> 
      let op = parse_unop s in
      let operand = parse_factor s in  (* Parse the inner expr *)
      Unop {op; operand}
  | Token.Ident name -> 
      ignore (take s);
      Var (name)
  | _ -> Fmt.failwith "parse error: invalid factor expr"
      

let rec parse_exp s min_prec =
  let left0 = parse_factor s in

  let rec loop left  =
    match peek s with
    | tok when precedence tok >= min_prec ->
        let op_tok = peek s in
        let op = parse_binop s in
        let right = parse_exp s (precedence op_tok + 1) in
        loop (Binop { op; lhs = left; rhs = right })
    | _ ->
        left
  in
  loop left0
    
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
  let value = parse_exp s 0 in 
  {op; name; value}

let parse_decl s = 
  match take s with
  | Token.Ident (sym) -> if Token.equal (peek s) (Token.Assign) then
                         let _ = take s in 
                         Init (sym, parse_exp s 0)
                         else New_var sym
  | tok -> Fmt.failwith "invalid declaration token: %s" (Token.show tok)

let parse_stm s =
  match take s with 
  | Token.Return -> 
      let e = parse_exp s 0 in 
      expect s Token.Semicolon;
      Return e
  | Token.Ident (sym) -> 
      let simp = parse_simp s sym in 
      expect s Token.Semicolon;
      Assign simp
  | Token.Int -> 
      let decl = parse_decl s in
      expect s Token.Semicolon;
      Declare decl
  | tok -> Fmt.failwith "invalid statement token: %s" (Token.show tok)

let parse_params s =
  let parse_param s =
  expect s Token.Int;
  match take s with
  | Token.Ident sym -> sym
  | tok -> Fmt.failwith "invalid param name, got %s" (Token.show tok)
  in 
  match peek s with
  | Token.RParen -> []
  | _ ->
      let p0 = parse_param s in
      let rec loop acc =
        match peek s with
        | Token.Comma ->
            ignore (take s); 
            let p = parse_param s in
            loop (p :: acc)
        | Token.RParen ->
            List.rev (p0 :: acc)
        | tok ->
            Fmt.failwith "invalid params: expected ',' or ')', got %s" (Token.show tok)
      in
      loop []

let parse_body (s : token_stream) : stm list =
  let rec loop acc =
    match peek s with
    | Token.RBrace -> List.rev acc
    | Token.Eof -> Fmt.failwith "unexpected EOF (missing '}')"
    | _ ->
        let st = parse_stm s in
        loop (st :: acc)
  in
  loop []

let parse_func s = 
  match take s with 
  | Token.Int ->
      let func_name = match take s with 
        | Token.Ident name -> name
        | tok -> Fmt.failwith "invalid function declaration token %s" (Token.show tok)
      in 
      expect s Token.LParen;
      let params =  parse_params s in
      expect s Token.RParen;
      expect s Token.LBrace;
      let body = parse_body s in
      {name = func_name; params; body} 
  | tok -> Fmt.failwith "invalid function type" (Token.show tok)
