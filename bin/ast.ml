type binop =
  | Plus
  | Minus
  | Times
  | Divide
  | Modulo

type asnop =
  | Assign
  | PlusEq
  | MinusEq
  | TimesEq
  | DivideEq
  | ModuloEq

type unop = Negative

type exp =
  | Var of Symbol.t
  | Const of Int32.t
  | Binop of
      { op : binop
      ; lhs : exp
      ; rhs : exp
      }
  | Unop of
      { op : unop
      ; operand : exp
      }

type simp = Asnop of 
              { op : asnop
              ; name: Symbol.t
              ; value: exp
              }

type decl =
  | Init of Symbol.t * exp
  | New_var of Symbol.t

type stm = 
  | Delcare of decl
  | Assign of simp 
  | Return of exp

type program = stm list