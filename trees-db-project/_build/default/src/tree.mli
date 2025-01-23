type 'a tree =
  | Node of 'a tree * 'a * 'a tree
  | Leaf

val tree_fold : ('a -> 'b -> 'a -> 'a) -> 'a -> 'b tree -> 'a
val map : 'a tree -> ('a -> 'b) -> 'b tree
val mirror : 'a tree -> 'a tree
val in_order : 'a tree -> 'a list
val pre_order : 'a tree -> 'a list
val compose : ('a -> 'a) tree -> 'a -> 'a
val depth : 'a tree -> int
val trim : 'a tree -> int -> 'a tree
(* Optional functions:
 * val tree_init : ('a -> ('a * 'b * 'a) option) -> 'a -> 'b tree
 * val split : 'a list -> 'a -> 'a list * 'a list
 *)
val from_pre_in : 'a list -> 'a list -> 'a tree
