type 'a tree =
  | Node of 'a tree * 'a * 'a tree
  | Leaf

let rec tree_fold f init tree = match tree with
Leaf -> init
| Node(l, x, r) -> let left = tree_fold f init l in let right = tree_fold f init r in 
f left x right

let map tree f = let map_helper l x r = Node(l, f x, r) in tree_fold map_helper Leaf tree

let mirror tree = let mirror_helper l x r = Node(r, x, l) in tree_fold mirror_helper Leaf tree

let in_order tree = let order_helper l x r = (l @ [x]) @ r in tree_fold order_helper [] tree

let pre_order tree  = let order_helper l x r = [x] @ l @ r in tree_fold order_helper [] tree

let compose tree = let compose_helper l x r = fun f -> r (x (l f)) in tree_fold compose_helper (fun f -> f) tree

let depth tree = let depth_helper l x r = 1 + (max l r) in tree_fold depth_helper 0 tree

(* Assume complete tree *)
let trim tree n = 
  let trim_helper l x r = 
    if n = 0 then Leaf
    else if n = 1 then Node(Leaf, x, Leaf)
    else Node(l, x, r)
  in tree_fold trim_helper Leaf tree

let rec tree_init f v = match f v with
| None -> Leaf
| Some (a, b, c) -> Node (tree_init f a, b, tree_init f c)

let rec split_helper lst v (l1, l2) = match lst with
| [] -> (l1, l2)
| h::t -> if h = v then (l1, l2 @ t) else split_helper t v (l1 @ [h], l2)  

let split lst v = split_helper lst v ([], [])

let rec split_by_val lst len count = match lst with
| [] -> []
| h::t -> if count < len then [h] @ (split_by_val t len (count + 1)) else [] 

let rec split_by_val_end lst len count = match lst with
| [] -> []
| h::t -> if count < len then (split_by_val_end t len (count + 1)) else h::t

let rec from_pre_in pre in_ord =
match pre with
| [] -> Leaf
| h::t ->
let left, right = split in_ord h in
let l_subtree = from_pre_in (split_by_val t (List.length left) 0) left in
let r_subtree = from_pre_in (split_by_val_end t (List.length left) 0) right in
Node(l_subtree, h, r_subtree)