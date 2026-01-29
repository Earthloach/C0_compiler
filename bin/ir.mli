type binop = Add | Sub | Mul | Div | Mod
type unop = Neg

type exp =
    Const of int32
  | Temp of int
  | Binop of { lhs : exp; op : binop; rhs : exp; }
  | Unop of { op : unop; operand : exp; }

type stm = Move of { dest : int; src : exp; } | Return of exp

type func = { name : Symbol.t; params : int list; body : stm list; }

type program = func list
