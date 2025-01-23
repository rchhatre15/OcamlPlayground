type person = { name: string;
                age: int;
                hobbies: string list }

(* Define the type of db below *)
type db = person list

let newDatabase = []

let insert person db = db @ [person]

let rec remove name db = match db with
| [] -> []
| h::t -> if h.name = name then (remove name t) else [h] @ (remove name t)

type condition =
  | True
  | False
  | Age of (int -> bool)
  | Name of (string -> bool)
  | Hobbies of (string list -> bool)
  | And of condition * condition
  | Or of condition * condition
  | Not of condition
  | If of condition * condition * condition

let rec query_helper c p =
  match c with
  | True -> true
  | False -> false
  | Age(f) -> f p.age
  | Name(f) -> f p.name
  | Hobbies(f) -> f p.hobbies
  | And(c1, c2) -> (query_helper c1 p) && (query_helper c2 p)
  | Or(c1, c2) -> (query_helper c1 p) || (query_helper c2 p)
  | Not(c1) -> not (query_helper c1 p)
  | If(c1, c2, c3) -> if (query_helper c1 p) then (query_helper c2 p) else (query_helper c3 p)

let rec query condition db = match db with 
| [] -> []
| h::t -> if (query_helper condition h) then [h] @ (query condition t) else (query condition t)

type comparator = person -> person -> int

let rec sort comparator db = List.sort comparator db

let queryBy condition db comparator = let sorted_db = (sort comparator db) in 
query condition sorted_db

let rec update condition db personData = match db with
| [] -> []
| h::t -> if (query_helper condition h) then [(personData h)] @ (update condition t personData) else [h] @ (update condition t personData)

let rec deleteAll condition db =  match db with
| [] -> []
| h::t -> if (query_helper condition h) then (deleteAll condition t) else [h] @ (deleteAll condition t)

