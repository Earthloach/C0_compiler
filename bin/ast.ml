type binop =
  | Add
  | Sub
  | Mul
  | Div
  | Mod

type asnop =
  | Eq
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

type simp = { op : asnop; name: Symbol.t; value: exp}

type decl =
  | Init of Symbol.t * exp
  | New_var of Symbol.t

type stm = 
  | Declare of decl
  | Assign of simp 
  | Return of exp

type func = 
  { name : Symbol.t
  ; params : Symbol.t list
  ; body : stm list
  }

type program = func list

(* Ast pretty printing *)
module Print = struct
  let pp_binop = function
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/"
    | Mod -> "%"
  ;;

  let pp_unop = function
    | Negative -> "-"
  ;;

  let pp_asnop : asnop -> string = function
    | Eq -> "="
    | PlusEq -> "+="
    | MinusEq -> "-="
    | TimesEq -> "*="
    | DivideEq -> "/="
    | ModuloEq -> "%="
  ;;

  let rec pp_exp = function
    | Var id -> Symbol.name id
    | Const c -> Int32.to_string c
    | Unop unop -> Format.sprintf "%s(%s)" (pp_unop unop.op) (pp_exp unop.operand)
    | Binop binop ->
      Format.sprintf "(%s %s %s)" (pp_exp binop.lhs) (pp_binop binop.op) (pp_exp binop.rhs)
  ;;


  let pp_decl = function
    | New_var id -> Format.sprintf "int %s;" (Symbol.name id)
    | Init (id, e) -> Format.sprintf "int %s = %s;" (Symbol.name id) (pp_exp e)
  ;;

  let pp_simp (simp : simp) = Format.sprintf "%s %s %s;" (Symbol.name simp.name) (pp_asnop simp.op) (pp_exp simp.value) 
  ;;

  let rec pp_stm = function
    | Declare d -> pp_decl d
    | Assign simp -> pp_simp simp 
    | Return e -> Format.sprintf "return %s;" (pp_exp e)

  and pp_stms stms = String.concat "" (List.map (fun stm -> pp_stm stm ^ "\n") stms)
  ;;

  let pp_func func = Format.sprintf "int %s(%s){\n%s}" 
    (Symbol.name func.name)
    (String.concat "" (List.map (fun param -> (Symbol.name param) ^ " " ) func.params)) 
    (pp_stms func.body)
end