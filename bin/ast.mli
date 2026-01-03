type binop = | Add | Sub | Mul | Div | Mod

type asnop = Eq | PlusEq | MinusEq | TimesEq | DivideEq | ModuloEq

type unop = Negative

type exp =
    Var of Symbol.t
  | Const of int32
  | Binop of { op : binop; lhs : exp; rhs : exp; }
  | Unop of { op : unop; operand : exp; }

type simp = { op : asnop; name : Symbol.t; value : exp; }

type decl = Init of Symbol.t * exp | New_var of Symbol.t

type stm = Declare of decl | Assign of simp | Return of exp

type func = 
  { name : Symbol.t
  ; params : Symbol.t list
  ; body : stm list
  }

type program = func list

module Print : sig
  val pp_func : func -> string
  val pp_decl : decl -> string
end
