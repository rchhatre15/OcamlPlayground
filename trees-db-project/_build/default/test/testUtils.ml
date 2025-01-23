open P2b.Tree
open OUnit2

let rec tree_printer node_printer = function
  |(Node(l,v,r)) ->
    let tree_printer = (fun _ -> tree_printer node_printer) in
    Printf.sprintf "Node(%a, %a, %a)" tree_printer l node_printer v tree_printer r
  | Leaf -> Printf.sprintf "Leaf"

let int_tree_printer =
  tree_printer (fun () -> Printf.sprintf "%d")

let string_tree_printer =
  tree_printer (fun () -> Printf.sprintf "%s")

let assert_true b = assert_equal true b
let assert_false b = assert_equal false b

