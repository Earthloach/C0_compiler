type token_stream =  
  { lex : Lexer.lexer
  ; mutable la : Token.t option
  }

val init_token_stream : string -> token_stream

val parse_unop : token_stream -> Ast.unop

val parse_binop : token_stream -> Ast.binop

val parse_factor : token_stream -> Ast.exp

val parse_exp : token_stream -> int -> Ast.exp

val parse_simp : token_stream -> Symbol.t -> Ast.simp

val parse_decl : token_stream -> Ast.decl

val parse_stm : token_stream -> Ast.stm

val parse_params : token_stream -> Symbol.t list

val parse_body : token_stream -> Ast.stm list

val parse_func : token_stream -> Ast.func
