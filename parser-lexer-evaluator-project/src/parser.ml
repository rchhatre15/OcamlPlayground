open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None

(* Part 2: Parsing expressions *)

let rec parse_expr toks = match (lookahead toks) with 
| Some Tok_Let -> parse_let toks
| Some Tok_Fun -> parse_fun toks
| Some Tok_If -> parse_if toks
| _ -> parse_or toks



and parse_let lst = match (lookahead lst) with
| Some Tok_Let -> let temp = (match_token lst Tok_Let) in begin match (lookahead temp) with
    | Some Tok_ID id -> let temp2 = (match_token temp (Tok_ID id)) in begin match (lookahead temp2) with
        | Some Tok_Equal ->
        let t = (match_token temp2 Tok_Equal) in
        let (t', exp) = (parse_expr t) in
        let t'' = (match_token t' Tok_In) in
        let (t''', exp') = (parse_expr t'') in
        (t''', Let (id, false, exp, exp'))
        | _ -> raise (InvalidInputException("mid fun")) end
    | Some Tok_Rec -> let temp2 = (match_token temp (Tok_Rec)) in begin match (lookahead temp2) with
        | Some Tok_ID id -> let temp3 = (match_token temp2 (Tok_ID id)) in begin match (lookahead temp3) with
            | Some Tok_Equal ->
            let t = (match_token temp3 Tok_Equal) in
            let (t', exp) = (parse_expr t) in
            let t'' = (match_token t' Tok_In) in
            let (t''', exp') = (parse_expr t'') in
            (t''', Let (id, true, exp, exp'))
            | _ -> raise (InvalidInputException("mid fun")) end
        | _ -> raise (InvalidInputException("mid fun")) end
    | _ -> raise (InvalidInputException("mid fun")) end
| _ -> raise (InvalidInputException("mid fun"))

and parse_fun lst = match (lookahead lst) with
| Some Tok_Fun -> let temp = (match_token lst Tok_Fun) in begin match (lookahead temp) with
    | Some Tok_ID id -> let temp2 = (match_token temp (Tok_ID id)) in begin match (lookahead temp2) with
        | Some Tok_Arrow ->
        let t = (match_token temp2 Tok_Arrow) in
        let (t', exp) = (parse_expr t) in
        (t', Fun (id, exp))
        | _ -> raise (InvalidInputException("mid fun")) end
    | _ -> raise (InvalidInputException("mid fun")) end
| _ -> raise (InvalidInputException("mid fun"))

and parse_if lst = match (lookahead lst) with
| Some Tok_If -> 
let t = (match_token lst Tok_If) in
let (t', exp) = (parse_expr t) in
let t'' = (match_token t' Tok_Then) in
let (t''', exp') = (parse_expr t'') in
let t'''' = (match_token t''' Tok_Else) in
let (t''''', exp'') = (parse_expr t'''') in
(t''''', If (exp, exp', exp''))
| _ -> raise (InvalidInputException("mid fun"))

and parse_or lst = let (t, exp) = (parse_and lst) in match (lookahead t) with
| Some Tok_Or -> 
let t' = (match_token t Tok_Or) in
let (t'', exp') = (parse_or t') in
(t'', Binop(Or, exp, exp'))
| _ -> parse_and lst

and parse_and lst = let (t, exp) = (parse_eq lst) in match (lookahead t) with
| Some Tok_And -> 
let t' = (match_token t Tok_And) in
let (t'', exp') = (parse_and t') in
(t'', Binop(And, exp, exp'))
| _ -> parse_eq lst

and parse_eq lst = let (t, exp) = (parse_rel lst) in match (lookahead t) with
| Some Tok_Equal -> 
let t' = (match_token t Tok_Equal) in
let (t'', exp') = (parse_eq t') in
(t'', Binop(Equal, exp, exp'))
| Some Tok_NotEqual -> 
let t' = (match_token t Tok_NotEqual) in
let (t'', exp') = (parse_eq t') in
(t'', Binop(NotEqual, exp, exp'))
| _ -> parse_rel lst

and parse_rel lst = let (t, exp) = (parse_add lst) in match (lookahead t) with
| Some Tok_Less -> 
let t' = (match_token t Tok_Less) in
let (t'', exp') = (parse_rel t') in
(t'', Binop(Less, exp, exp'))
| Some Tok_LessEqual -> 
let t' = (match_token t Tok_LessEqual) in
let (t'', exp') = (parse_rel t') in
(t'', Binop(LessEqual, exp, exp'))
| Some Tok_Greater -> 
let t' = (match_token t Tok_Greater) in
let (t'', exp') = (parse_rel t') in
(t'', Binop(Greater, exp, exp'))
| Some Tok_GreaterEqual -> 
let t' = (match_token t Tok_GreaterEqual) in
let (t'', exp') = (parse_rel t') in
(t'', Binop(GreaterEqual, exp, exp'))
| _ -> parse_add lst

and parse_add lst = let (t, exp) = (parse_mult lst) in match (lookahead t) with
| Some Tok_Add -> 
let t' = (match_token t Tok_Add) in
let (t'', exp') = (parse_add t') in
(t'', Binop(Add, exp, exp'))
| Some Tok_Sub ->
let t' = (match_token t Tok_Sub) in
let (t'', exp') = (parse_add t') in
(t'', Binop(Sub, exp, exp'))
| _ -> parse_mult lst

and parse_mult lst = let (t, exp) = (parse_concat lst) in match (lookahead t) with
| Some Tok_Mult -> 
let t' = (match_token t Tok_Mult) in
let (t'', exp') = (parse_mult t') in
(t'', Binop(Mult, exp, exp'))
| Some Tok_Div ->
let t' = (match_token t Tok_Div) in
let (t'', exp') = (parse_mult t') in
(t'', Binop(Div, exp, exp'))
| _ -> parse_concat lst

and parse_concat lst = let (t, exp) = (parse_unary lst) in match (lookahead t) with
| Some Tok_Concat -> 
let t' = (match_token t Tok_Concat) in
let (t'', exp') = (parse_concat t') in
(t'', Binop(Concat, exp, exp'))
| _ -> parse_unary lst

and parse_unary lst = match (lookahead lst) with
| Some Tok_Not -> 
let t = (match_token lst Tok_Not) in 
let (t', exp) = (parse_unary t) in
(t', Not(exp))
| _ -> parse_fxncall lst

and parse_fxncall lst = let (t, exp) = (parse_primary lst) in match (lookahead t) with
| Some Tok_Int _ | Some Tok_Bool _ | Some Tok_String _ | Some Tok_ID _ | Some Tok_LParen -> 
let (t', exp') = (parse_fxncall t) in (t', FunctionCall(exp, exp'))
| _ -> (t, exp)

and parse_primary lst = match (lookahead lst) with
| Some Tok_Int i -> ((match_token lst (Tok_Int i)), Value(Int i))
| Some Tok_Bool b -> ((match_token lst (Tok_Bool b)), Value(Bool b))
| Some Tok_String s -> ((match_token lst (Tok_String s)), Value(String s))
| Some Tok_ID id -> ((match_token lst (Tok_ID id)), ID id)
| Some Tok_LParen -> 
let t = (match_token lst Tok_LParen) in
let (t', exp) = (parse_expr t) in
let t'' = (match_token t' Tok_RParen) in 
(t'', exp)
| _ -> raise (InvalidInputException("primary is mid"))
            

(* Part 3: Parsing mutop *)

let rec remove_last lst =match lst with
| [] -> []
| [_] -> []
| h::t -> h::remove_last t

let rec parse_mutop toks = match (lookahead toks) with 
| Some Tok_Def -> parse_def toks
| Some Tok_DoubleSemi -> ([], NoOp)
| _ -> parse_mutopexpr toks

and parse_mutopexpr lst = if (lookahead_many lst ((List.length lst)-1)) = Some Tok_DoubleSemi 
then let (t,exp) = parse_expr (remove_last lst) in (t, Expr(exp))
else raise(InvalidInputException("mid mutop"))


and parse_def lst = 
if (lookahead_many lst ((List.length lst)-1)) = Some Tok_DoubleSemi 
then let lst = (remove_last lst) in match (lookahead lst) with
| Some Tok_Def -> let temp = (match_token lst Tok_Def) in begin match (lookahead temp) with
    | Some Tok_ID id -> let temp2 = (match_token temp (Tok_ID id)) in begin match (lookahead temp2) with
        | Some Tok_Equal ->
        let t = (match_token temp2 Tok_Equal) in
        let (t', exp) = (parse_expr t) in
        (t', Def (id, exp))
        | _ -> raise (InvalidInputException("mid def")) end
    | _ -> raise (InvalidInputException("mid def")) end
| _ -> raise (InvalidInputException("mid def"))
else raise(InvalidInputException("mid mutop"))
