open TokenTypes

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

let tokenize input = 

let rec tok_aux input pos =  
if pos >= (String.length input) then []  

else if (Str.string_match (Str.regexp "=") input pos) then [Tok_Equal] @ (tok_aux input (pos+1))  
else if (Str.string_match (Str.regexp "<>") input pos) then [Tok_NotEqual] @ (tok_aux input (pos+2))
else if (Str.string_match (Str.regexp ">") input pos) then [Tok_Greater] @ (tok_aux input (pos+1))
else if (Str.string_match (Str.regexp "<") input pos) then [Tok_Less] @ (tok_aux input (pos+1))
else if (Str.string_match (Str.regexp ">=") input pos) then [Tok_GreaterEqual] @ (tok_aux input (pos+2))
else if (Str.string_match (Str.regexp "<=") input pos) then [Tok_LessEqual] @ (tok_aux input (pos+2))
else if (Str.string_match (Str.regexp "||") input pos) then [Tok_Or] @ (tok_aux input (pos+2))
else if (Str.string_match (Str.regexp "&&") input pos) then [Tok_And] @ (tok_aux input (pos+2)) 
else if (Str.string_match (Str.regexp "not ") input pos) then [Tok_Not] @ (tok_aux input (pos+4)) 
else if (Str.string_match (Str.regexp "if ") input pos) then [Tok_If] @ (tok_aux input (pos+3)) 
else if (Str.string_match (Str.regexp "then ") input pos) then [Tok_Then] @ (tok_aux input (pos+5)) 
else if (Str.string_match (Str.regexp "else ") input pos) then [Tok_Else] @ (tok_aux input (pos+5)) 
else if (Str.string_match (Str.regexp "+") input pos) then [Tok_Add] @ (tok_aux input (pos+1))
else if (Str.string_match (Str.regexp "*") input pos) then [Tok_Mult] @ (tok_aux input (pos+1))
else if (Str.string_match (Str.regexp "/") input pos) then [Tok_Div] @ (tok_aux input (pos+1))
else if (Str.string_match (Str.regexp "\\^") input pos) then [Tok_Concat] @ (tok_aux input (pos+1))
else if (Str.string_match (Str.regexp "let ") input pos) then [Tok_Let] @ (tok_aux input (pos+4))
else if (Str.string_match (Str.regexp "rec ") input pos) then [Tok_Rec] @ (tok_aux input (pos+4))
else if (Str.string_match (Str.regexp "in ") input pos) then [Tok_In] @ (tok_aux input (pos+3))
else if (Str.string_match (Str.regexp "def ") input pos) then [Tok_Def] @ (tok_aux input (pos+4))
else if (Str.string_match (Str.regexp "fun ") input pos) then [Tok_Fun] @ (tok_aux input (pos+4))
else if (Str.string_match (Str.regexp "->") input pos) then [Tok_Arrow] @ (tok_aux input (pos+2))
else if (Str.string_match (Str.regexp ";;") input pos) then [Tok_DoubleSemi] @ (tok_aux input (pos+2))

else if (Str.string_match (Str.regexp "true\\|false") input pos) then let str = Str.matched_string input in
if str = "true" then [Tok_Bool true] @ (tok_aux input (pos+4)) else [Tok_Bool false] @ (tok_aux input (pos+5))

else if (Str.string_match (Str.regexp "[0-9]+") input pos) then let num = Str.matched_string input in
[Tok_Int (int_of_string num)] @ ((tok_aux input (pos+(String.length num))))

else if (Str.string_match (Str.regexp "\"[^\"]*\"") input pos) then let str = Str.matched_string input in
[Tok_String (String.sub str 1 ((String.length str)-2))] @ ((tok_aux input (pos+(String.length str))))

else if (Str.string_match (Str.regexp ")") input pos) then [Tok_RParen] @ (tok_aux input (pos+1)) 

else if (Str.string_match (Str.regexp "(") input pos) then
if (Str.string_match (Str.regexp "-[0-9]+") input (pos+1)) then let num = Str.matched_string input in 
[Tok_Int (int_of_string num)] @ ((tok_aux input (pos+(String.length num)+2)))
else [Tok_LParen] @ (tok_aux input (pos+1))

else if (Str.string_match (Str.regexp "-") input pos) then [Tok_Sub] @ (tok_aux input (pos+1))

else if (Str.string_match (Str.regexp "[a-zA-Z][a-zA-Z0-9]*") input pos)
then let str = Str.matched_string input in
[Tok_ID str] @ ((tok_aux input (pos+(String.length str))))

else (tok_aux input (pos+1))

in tok_aux input 0

