open OUnit2
open TestUtils
open P2b.Tree

let public_tree_init _ =
  let inc_tree =
    tree_init
      (fun a ->
        if a <= 3 then
          Some (a+1, a, a+1)
        else None)
      1 in

  let inc_tree2 =
    tree_init
      (fun a ->
        if a <= 3 then
          Some (a+1, a, a+2)
        else None)
      1 in
  assert_equal
    (Node(Node(Node(Leaf, 3, Leaf), 2, Node(Leaf, 3, Leaf)), 1, Node(Node(Leaf, 3, Leaf), 2, Node(Leaf, 3, Leaf))))
    inc_tree;
  assert_equal (Node(Node(Node(Leaf, 3, Leaf), 2, Leaf), 1, Node(Leaf, 3, Leaf)))
    inc_tree2

let public_split _ =
  let l1 = [1; 2; 3; 4] in
  let l2 = ["Hello"; "World"] in

  assert_equal
    ([1], [3; 4]) @@
    split l1 2;

  assert_equal
    (["Hello"], []) @@
    split l2 "World"

let tree4 = Node (Leaf, (fun x -> x +1), Node (Leaf, (fun x -> 2/x), Leaf))

let semipublic_tree_init _ =
  assert_equal
    ~printer: int_tree_printer
    (Leaf) @@ tree_init (fun _ -> None) 1;
  assert_equal
    ~printer: int_tree_printer
    (Node (Leaf, 5, Leaf)) @@ tree_init (fun a -> if a = 4 then None else if a = 6 then None else Some (a-1, a, a+1)) 5

let semipublic_split _ =
  assert_equal ([], [1; 1; 1]) @@ split [1;1;1;1] 1;
  assert_equal ([1], [1; 2; 3]) @@ split [1;2;1;2;3] 2;
  assert_equal ([1; 2; 1; 2; 3], []) @@ split [1;2;1;2;3] 5;
  assert_equal ([], []) @@ split [] 1

let secret_tree_init _ =
  assert_equal
  (Node (Node (Leaf, Leaf, Leaf), Leaf, Node (Node (Leaf, Leaf, Leaf), Leaf, Leaf)))
  @@ tree_init (fun a -> match a with
              | Node (l,_,r) -> Some (l, Leaf, r)
              | Leaf -> None) 
      (Node (Node (Leaf, 1, Leaf), 0, Node (Node (Leaf, 3, Leaf), 2, Leaf)))
