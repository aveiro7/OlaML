module GrammarRules
type name = string

type CompareRelation = Equal | Greater | GreaterEq | Less | LessEq

type expr = 
    | Var of name
    | Int of int
    | Float of float
    | String of string
    | Bool of bool
    | Call of expr * expr list
    | Fun of name list * expr
    | Let of name * expr
    | LetIn of name * expr * expr
    | Sum of expr * expr
    | FloatSum of expr * expr
    | Diff of expr * expr
    | FloatDiff of expr * expr
    | Mul of expr * expr
    | FloatMul of expr * expr
    | Div of expr * expr
    | FloatDiv of expr * expr
    | Concat of expr * expr
    | IfElse of expr * expr * expr
    | Not of expr
    | Compare of expr * CompareRelation * expr
    | List of expr list
    | Tuple of expr list
    | ListConcat of expr * expr

type id = int
type level = int

type ty = 
    | TInt
    | TFloat
    | TString
    | TBool
    | TList of ty
    | TTuple of ty list
    | TApp of ty * ty list
    | TArrow of ty list * ty
    | TVar of tvar ref

and tvar =
    | Unbound of id * level
    | Link of ty
    | Generic of id

let rec string_of_ty = function
    | TVar {contents = Link ty } -> "var : " + string_of_ty ty
    | TArrow(param_ty_list, return_ty) -> "function" 
    | TApp(ty, ty_arg_list) -> "function application"
    | TVar {contents = Unbound(id, _)} -> "_" + string(id)
    | TVar {contents = Generic _} -> "generic type"
    | TInt -> "int"
    | TFloat -> "float"
    | TBool -> "bool"
    | TString -> "string"
    | TList ty -> "[" + (string_of_ty ty) + "]"
    | TTuple ty_list -> (List.fold (fun acc x -> acc + " * " + (string_of_ty x)) "(Tuple: " ty_list) + ")"
    | _ -> "unknown type"