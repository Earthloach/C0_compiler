type binop =
  | Add
  | Sub
  | Mul
  | Div
  | Mod

type unop = Neg

type exp =
  | Const of Int32.t
  | Temp of Temp.t
  | Binop of
      { lhs : exp
      ; op : binop
      ; rhs : exp
      }
  | Unop of 
      { op : unop
      ; operand : exp
      }

type stm =
  | Move of
      { dest : Temp.t
      ; src : exp
      }
  | Return of exp

type func = 
      { name : Symbol.t 
      ; params : Temp.t list 
      ; body : stm list 
      }

type program = func list
