module TypeInference

open GrammarRules
open System.Collections
exception TypeError of string

let error msg = raise (TypeError msg)

module Env = begin
    module StringMap = Map

    let empty = StringMap.empty
    let extend env name ty = StringMap.add name ty env
    let lookup env name = StringMap.find name env
end

let current_id = ref 0

let next_id () =
    let id = !current_id in
    current_id := id + 1;
    id

let reset_id () = current_id := 0

let new_var level = TVar (ref (Unbound(next_id (), level)))
let new_gen_var () = TVar (ref (Generic(next_id ())))

let occurs_check_adjust_levels tvar_id tvar_level ty =
    let rec f = function
        | TVar {contents = Link ty} -> f ty
        | TVar {contents = Generic _} -> assert false
        | TVar ({contents = Unbound(other_id, other_level)} as other_tvar) ->
            if other_id = tvar_id then
                error "recursive types"
            else
                if other_level > tvar_level then
                    other_tvar := Unbound(other_id, tvar_level)
                else ()
        | TApp(ty, ty_arg_list) ->
            f ty;
            List.iter f ty_arg_list
        | TArrow(param_ty_list, return_ty) ->
            List.iter f param_ty_list ;
            f return_ty
        | _ -> ()
    in
    f ty

let rec unify ty1 ty2 =
    if ty1.Equals(ty2) then () else
    match (ty1, ty2) with
        | TApp(ty1, ty_arg_list1), TApp(ty2, ty_arg_list2) ->
            unify ty1 ty2 ;
            List.iter2 unify ty_arg_list1 ty_arg_list2
        | TArrow(param_ty_list1, return_ty1), TArrow(param_ty_list2, return_ty2) ->
            List.iter2 unify param_ty_list1 param_ty_list2 ;
            unify return_ty1 return_ty2
        | TVar {contents = Link ty1}, ty2 | ty1, TVar {contents = Link ty2} -> unify ty1 ty2
        | TVar {contents = Unbound(id1, _)}, TVar {contents = Unbound(id2, _)} when id1 = id2 ->
            assert false (* There is only a single instance of a particular type variable. *)
        | TVar ({contents = Unbound(id, level)} as tvar), ty
        | ty, TVar ({contents = Unbound(id, level)} as tvar) ->
            occurs_check_adjust_levels id level ty ;
            tvar := Link ty
        | _, _ -> error ("cannot unify types " + string_of_ty ty1 + " and " + string_of_ty ty2)

let rec generalize level = function
    | TVar {contents = Unbound(id, other_level)} when other_level > level ->
        TVar (ref (Generic id))
    | TApp(ty, ty_arg_list) ->
        TApp(generalize level ty, List.map (generalize level) ty_arg_list)
    | TArrow(param_ty_list, return_ty) ->
        TArrow(List.map (generalize level) param_ty_list, generalize level return_ty)
    | TVar {contents = Link ty} -> generalize level ty
    | TVar {contents = Generic _} as ty -> ty
    | TVar {contents = Unbound(x, y)} -> TVar {contents = Generic x}
    | TTuple(arg_list) -> TTuple(List.map (generalize level) arg_list)
    | TInt | TFloat | TString | TBool as ty -> ty
    | _ as ty -> ty

let instantiate level ty =
    let id_var_map = ref Map.empty in
        let rec f ty = match ty with
            | TVar {contents = Link ty} -> f ty
            | TVar {contents = Generic id} -> 
                begin
                try
                    Map.find id !id_var_map
                with Not_found ->
                    let var = new_var level in
                        id_var_map := Map.add id var !id_var_map;
                    var
                end
            | TVar {contents = Unbound _} -> ty
            | TApp(ty, ty_arg_list) ->
                TApp(f ty, List.map f ty_arg_list)
            | TArrow(param_ty_list, return_ty) ->
                TArrow(List.map f param_ty_list, f return_ty)
            | _ as ty -> ty
        in
        f ty

let rec match_fun_ty num_params = function
    | TArrow(param_ty_list, return_ty) ->
        if List.length param_ty_list <> num_params then
            error "unexpected number of arguments"
        else
            param_ty_list, return_ty
    | TVar {contents = Link ty} -> match_fun_ty num_params ty
    | TVar ({contents = Unbound(id, level)} as tvar) ->
        let param_ty_list = 
            let rec f = function
                | 0 -> []
                | n -> new_var level :: f (n - 1)
            in
                f num_params
        in
            let return_ty = new_var level in
                tvar := Link (TArrow(param_ty_list, return_ty)) ;
                param_ty_list, return_ty
    | _ -> error "expected a function"

let rec infer env level = function
    | Var name -> 
        begin
        try
            let looked_up = Env.lookup env name in
            let x = instantiate level looked_up in
                x
        with Not_found -> error ("variable " + name + " not found.")
        end
    | Int value -> TInt
    | Float value -> TFloat
    | String value -> TString
    | Bool value -> TBool
    | Call(fn_expr, arg_list) ->  
        let param_ty_list, return_ty = match_fun_ty (List.length arg_list) (infer env level fn_expr) in
            List.iter2 (fun param_ty arg_expr -> unify param_ty (infer env level arg_expr)) param_ty_list arg_list ; 
        return_ty
    | Fun(param_list, expr) -> 
        let param_ty_list = List.map (fun _ -> new_var level) param_list
        let fn_env = List.fold2 (fun env param_name param_ty -> Env.extend env param_name param_ty) env param_list param_ty_list
        let return_ty = infer fn_env level expr in
            TArrow(param_ty_list, return_ty)
    | Let(var_name, expr) -> 
        let var_ty = infer env level expr
        let generalized_ty = generalize level var_ty
        let env = Env.extend env var_name generalized_ty in
            generalized_ty
    | LetIn(var_name, value_expr, body_expr) ->
        let var_ty = infer env level value_expr
        let generalized_ty = generalize level var_ty in
            infer (Env.extend env var_name generalized_ty) (level + 1) body_expr
    | Sum(exp1, exp2) | Diff(exp1, exp2) | Mul(exp1, exp2) | Div(exp1, exp2) -> 
        let exp1_ty = infer env level exp1
        let exp2_ty = infer env level exp2 in
            unify exp1_ty exp2_ty;
            unify exp1_ty TInt ;
        TInt
    | FloatSum(exp1, exp2) | FloatDiff(exp1, exp2) | FloatMul(exp1, exp2) | FloatDiv(exp1, exp2) -> 
        let exp1_ty = infer env (level + 1) exp1
        let exp2_ty = infer env (level + 1) exp2 in
            unify exp1_ty exp2_ty;
            unify exp1_ty TFloat ;
        TFloat
    | Concat(exp1, exp2) -> 
        let exp1_ty = infer env level exp1
        let exp2_ty = infer env level exp1 in
            unify exp1_ty exp2_ty;
            unify exp1_ty TString;
        TString
    | IfElse(cond, exp1, exp2) -> 
        let cond_ty = infer env level cond 
        let exp1_ty = infer env level exp1
        let exp2_ty = infer env level exp2 in
            unify cond_ty TBool;
            unify exp1_ty exp2_ty;
            exp1_ty
    | Not exp ->
        let exp_ty = infer env level exp in
            unify exp_ty TBool;
        TBool
    | Compare(exp1, _, exp2) ->
        let exp1_ty = infer env level exp1
        let exp2_ty =  infer env level exp2 in
            unify exp1_ty exp2_ty;
        TBool
    | List elements -> 
        let ty = infer env level (List.head elements) in
            List.iter (fun elem -> unify ty (infer env level elem)) (List.tail elements);
        TList(ty)
    | Tuple elements -> 
        let elements_ty = List.fold (fun acc elem -> (infer env level elem) :: acc ) [] elements in
        TTuple(List.rev elements_ty)
    | ListConcat (list1, list2) -> 
        let list1_ty = infer env level list1  
        let list2_ty = infer env level list2 in
            unify list1_ty list2_ty;
        list1_ty
            
