open MicroCamlTypes
open Utils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError 

(* Provided functions
  Helpful for creating an environment. You can do this with references 
  (not taught) or without. 
  You do not have to use these and you can modify them if you want. 
  If you do not use references, you will need the following data type:
*)
(*type values = Int of int|Bool of bool|String of string*)

(* Adds mapping [x:v] to environment [env] *)
let ref_extend env x v = (x, ref v)::env

(*let extend env x v = (x,v)::env*)

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec ref_lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then !value else ref_lookup t x

(*let rec lookup env x = 
  match env with
  [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then value else lookup t x*)

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let ref_extend_tmp env x = (x, ref (Int 0))::env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec ref_update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then (value := v) else ref_update t x v
        
(* Removes the most recent variable,value binding from the environment *)
(*let rec remove env x = match env with
  [] -> []
  | (var,value)::t -> if x = var then t else (var,value)::(remove t x)*)




(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning a value, or throwing an exception on error *)
let rec eval_expr env e = match e with

| Value v -> v

| ID id -> ref_lookup env id

| Not e -> let bool = eval_expr env e in begin match bool with
    | Bool b -> Bool (not b)
    | _ -> raise (TypeError ("Expected type bool")) end

| Binop (Add, e1, e2) -> let v1 = eval_expr env e1 in let v2 = eval_expr env e2 
in begin match (v1, v2) with
    | (Int e1, Int e2) -> Int (e1 + e2)
    | _ -> raise (TypeError ("Expected type int")) end

| Binop (Sub, e1, e2) -> let v1 = eval_expr env e1 in let v2 = eval_expr env e2 
in begin match (v1, v2) with
    | (Int e1, Int e2) -> Int (e1 - e2)
    | _ -> raise (TypeError ("Expected type int")) end

| Binop (Mult, e1, e2) -> let v1 = eval_expr env e1 in let v2 = eval_expr env e2 
in begin match (v1, v2) with
    | (Int e1, Int e2) -> Int (e1 * e2)
    | _ -> raise (TypeError ("Expected type int")) end

| Binop (Div, e1, e2) -> let v1 = eval_expr env e1 in let v2 = eval_expr env e2 
in begin match (v1, v2) with
    | (Int e1, Int e2) -> if e2 = 0 then raise (DivByZeroError) else Int (e1 / e2)
    | _ -> raise (TypeError ("Expected type int")) end

| Binop (Greater, e1, e2) -> let v1 = eval_expr env e1 in let v2 = eval_expr env e2 
in begin match (v1, v2) with
    | (Int e1, Int e2) -> Bool (e1 > e2)
    | _ -> raise (TypeError ("Expected type int")) end

| Binop (Less, e1, e2) -> let v1 = eval_expr env e1 in let v2 = eval_expr env e2 
in begin match (v1, v2) with
    | (Int e1, Int e2) -> Bool (e1 < e2)
    | _ -> raise (TypeError ("Expected type int")) end

| Binop (GreaterEqual, e1, e2) -> let v1 = eval_expr env e1 in let v2 = eval_expr env e2 
in begin match (v1, v2) with
    | (Int e1, Int e2) -> Bool (e1 >= e2)
    | _ -> raise (TypeError ("Expected type int")) end

| Binop (LessEqual, e1, e2) -> let v1 = eval_expr env e1 in let v2 = eval_expr env e2 
in begin match (v1, v2) with
    | (Int e1, Int e2) -> Bool (e1 <= e2)
    | _ -> raise (TypeError ("Expected type int")) end

| Binop (Concat, e1, e2) -> let v1 = eval_expr env e1 in let v2 = eval_expr env e2 
in begin match (v1, v2) with
    | (String e1, String e2) -> String (e1 ^ e2)
    | _ -> raise (TypeError ("Expected type string")) end

| Binop (Equal, e1, e2) -> let v1 = eval_expr env e1 in let v2 = eval_expr env e2 
in begin match (v1, v2) with
    | (String e1, String e2) -> Bool (e1 = e2)
    | (Int e1, Int e2) -> Bool (e1 = e2)
    | (Bool e1, Bool e2) -> Bool (e1 = e2)
    | _ -> raise (TypeError ("Cannot compare types")) end

| Binop (NotEqual, e1, e2) -> let v1 = eval_expr env e1 in let v2 = eval_expr env e2 
in begin match (v1, v2) with
    | (String e1, String e2) -> Bool (e1 <> e2)
    | (Int e1, Int e2) -> Bool (e1 <> e2)
    | (Bool e1, Bool e2) -> Bool (e1 <> e2)
    | _ -> raise (TypeError ("Cannot compare types")) end

| Binop (Or, e1, e2) -> let v1 = eval_expr env e1 in let v2 = eval_expr env e2 
in begin match (v1, v2) with
    | (Bool e1, Bool e2) -> Bool (e1 || e2)
    | _ -> raise (TypeError ("Cannot compare types")) end

| Binop (And, e1, e2) -> let v1 = eval_expr env e1 in let v2 = eval_expr env e2 
in begin match (v1, v2) with
    | (Bool e1, Bool e2) -> Bool (e1 && e2)
    | _ -> raise (TypeError ("Cannot compare types")) end
| If (i, t, e) -> let v1 = eval_expr env i in begin match v1 with
    | Bool true -> eval_expr env t
    | Bool false -> eval_expr env e
    | _ -> raise (TypeError ("Condition must be boolean")) end


| Let (id, false, e1, e2) -> let v1 = eval_expr env e1 in let v2 = ref_extend env id v1
in eval_expr v2 e2

| Let (id, true, e1, e2) -> 
    let temp_env = ref_extend_tmp env id in
    let v1 = eval_expr temp_env e1 in
    let _ = ref_update temp_env id v1 in
    eval_expr temp_env e2

| Fun (id, e) -> Closure(env, id, e)


| FunctionCall(e1, e2) -> begin match (eval_expr env e1) with 
    | Closure(a, x, e) -> eval_expr (ref_extend a x (eval_expr env e2)) e
    | _ -> raise (TypeError ("Not a function")) end




(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m = match m with

| Def (var, e) -> 
([(var, {contents = (eval_expr (ref_extend_tmp env var) e)})], Some (eval_expr (ref_extend_tmp env var) e))

| Expr e -> (env, Some (eval_expr env e))

| NoOp -> (env, None)
