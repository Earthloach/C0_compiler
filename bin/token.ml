type t = 
  (* keywords *)
  | Int 
  | Return 
  | Struct
  | Typedef
  | If
  | Else
  | While
  | For
  | Continue
  | Break
  | Assert
  | True
  | False
  | Null
  | Alloc
  | Alloc_array
  | Bool
  | Void
  | Char
  | String

  | LBrace 
  | RBrace
  | LParen 
  | RParen
  | Plus
  | Minus
  | Assign
  | Star
  | Slash 
  | Percent
  | PlusEq
  | MinusEq
  | SlashEq
  | PercentEq
  | StarEq
  | EqEq
  | Minus_minus
  | Semicolon
  | Dec_const of Int32.t
  | Hex_const of Int32.t 
  | Ident of Symbol.t
  | Comment of string
  | Eof
[@@deriving show, eq, ord]