open OUnit2
open TestUtils
open P2b.Tree
open P2b.Db

(* PART 1: DATABASE *)

let person1 = {name="Alice";age=23;hobbies=["Skiing";"golfing"]}
let person2 = {name="Bob";age=42;hobbies=["Skiing";"Cooking"; "Legos"]}
let person3 = {name="Clyff";age=98;hobbies=["Legos";"Gaming"]}
let person4 = {name="Duncan";age=13;hobbies=["Gaming";"DnD"]}

let condition_empty = True
let condition_name = Name (fun name -> name = "Clyff")
let condition_age = Age (fun age -> age > 30)
let condition_hobbies = Hobbies (fun hobbies -> List.mem "Gaming" hobbies)
let condition_and = And (Age (fun age -> age > 41), Age(fun age -> age < 43))
let condition_or = Or (Hobbies (fun hobbies -> List.mem "DnD" hobbies), Name(fun name -> String.length name < 4))
let condition_not = Not (condition_name)
let condition_if = If (condition_age, condition_name, condition_not)

let comparator_name = (fun p1 p2 -> 
  if p1.age < p2.age then 
    -1 
  else if p1.age = p2.age then 
    0 
  else 
    1)

let change_name person = { name = "Luke"; age = person.age ; hobbies = person.hobbies }

let db1 = insert person1 newDatabase

let db2 = insert person4 (insert person3 (insert person2 (insert person1 newDatabase)))

let public_insert1 _ =
  let applied = query condition_empty db1 in

  assert_equal 1 @@ List.length applied;
  assert_true (List.mem person1 applied)

let public_remove1 _ =
  let db1 = remove person1.name db1 in
  let applied = query condition_empty db1 in

  assert_equal 0 @@ List.length applied

let public_remove2 _ =
  let db2 = remove person2.name db2 in
  let db2 = remove person4.name db2 in
  let applied = query condition_empty db2 in

  assert_equal 2 @@ List.length applied;
  assert_true (List.mem person1 applied);
  assert_false (List.mem person2 applied);
  assert_true (List.mem person3 applied)
  
let public_query1 _ = 
  let applied = query condition_name db2 in
  
  assert_equal 1 @@ List.length applied;
  assert_true (List.mem person3 applied);

  let applied = query condition_age db2 in
  assert_equal 2 @@ List.length applied;
  assert_true (List.mem person2 applied);
  assert_true (List.mem person3 applied);

  let applied = query condition_hobbies db2 in
  assert_equal 2 @@ List.length applied;
  assert_true (List.mem person3 applied);
  assert_true (List.mem person4 applied)

let public_query2 _ =
  let applied = query condition_and db2 in

  assert_equal 1 @@ List.length applied;
  assert_true (List.mem person2 applied);

  let applied = query condition_or db2 in

  assert_equal 2 @@ List.length applied;
  assert_true (List.mem person2 applied);
  assert_true (List.mem person4 applied);

  let applied = query condition_not db2 in 

  assert_equal 3 @@ List.length applied;
  assert_true (List.mem person1 applied);
  assert_true (List.mem person2 applied);
  assert_true (List.mem person4 applied);

  let applied = query condition_if db2 in

  assert_equal 3 @@ List.length applied;
  assert_true (List.mem person1 applied);
  assert_false (List.mem person2 applied);
  assert_true (List.mem person3 applied);
  assert_true (List.mem person4 applied)

let public_sort _ =  
  let sorted = sort comparator_name db2 in

  assert_equal [person4; person1; person2; person3] sorted

let public_queryby _ =   
  let applied = queryBy condition_age db2 comparator_name in

  assert_equal [person2; person3] applied

let public_update _ =
  let updated = update condition_name db2 change_name in
  let applied = query condition_empty updated in 
  
  assert_equal 4 @@ List.length applied;
  assert_true (List.mem {name = "Luke"; age = 98; hobbies = ["Legos";"Gaming"]} applied);
  assert_true (List.mem person1 applied);
  assert_true (List.mem person2 applied);
  assert_true (List.mem person4 applied)

let public_deleteall _ =
  let applied = deleteAll condition_age db2 in
  let applied = query condition_empty applied in

  assert_equal 2 @@ List.length applied;
  assert_true (List.mem person1 applied);
  assert_true (List.mem person4 applied)

(* PART 2/3: TREE*)

let public_tree_fold _ =
  let treea = Node(Node(Leaf, 1, Leaf), 2, Node(Leaf, 3, Leaf)) in
  let treeb = Node(Node(Leaf, 1, Leaf), 2, Node(Node(Leaf, 3, Leaf), 4, Leaf)) in

  assert_equal 6 @@ tree_fold (fun a b c -> a+b+c) 0 treea;
  assert_equal 10 @@ tree_fold (fun a b c -> a+b+c) 0 treeb

let public_map _ =
  let treea = Node(Node(Leaf, 1, Leaf), 2, Node(Leaf, 3, Leaf)) in
  let treeb = Node(Node(Leaf, 1, Leaf), 2, Node(Node(Leaf, 3, Leaf), 4, Leaf)) in

  assert_equal
    ~printer: string_tree_printer
    (Node(Node(Leaf, "1", Leaf), "2", Node(Leaf, "3", Leaf))) @@ map treea string_of_int;
  assert_equal
    ~printer: int_tree_printer
    (Node(Node(Leaf, 2, Leaf), 3, Node(Node(Leaf, 4, Leaf), 5, Leaf))) @@  map treeb succ

let public_mirror _ =
  let treea = Node(Node(Leaf, 1, Leaf), 2, Node(Leaf, 3, Leaf)) in
  let treeb = Node(Node(Leaf, 1, Leaf), 2, Node(Node(Leaf, 3, Leaf), 4, Leaf)) in

  assert_equal
    ~printer: int_tree_printer
    (Node(Node(Leaf, 3, Leaf), 2, Node(Leaf, 1, Leaf))) @@
    mirror treea;

  assert_equal
    ~printer: int_tree_printer
    (Node(Node(Leaf, 4, Node(Leaf, 3, Leaf)), 2, Node(Leaf, 1, Leaf))) @@
    mirror treeb

let public_in_order _ =
  let treea = Node(Node(Leaf, 1, Leaf), 2, Node(Leaf, 3, Leaf)) in
  let treeb = Node(Node(Leaf, 1, Leaf), 2, Node(Node(Leaf, 3, Leaf), 4, Leaf)) in

  assert_equal
    ~msg:"in_order(1)"
    [1; 2; 3] @@
    in_order treea;

  assert_equal
    ~msg:"in_order(2)"
    [1; 2; 3; 4] @@
    in_order treeb

let public_pre_order _ =
  let treea = Node(Node(Leaf, 1, Leaf), 2, Node(Leaf, 3, Leaf)) in
  let treeb = Node(Node(Leaf, 1, Leaf), 2, Node(Node(Leaf, 3, Leaf), 4, Leaf)) in

  assert_equal
    ~msg:"pre_order(1)"
    [2; 1; 3] @@
    pre_order treea;

  assert_equal
    ~msg:"pre_order(2)"
    [2; 1; 4; 3] @@
    pre_order treeb

let public_compose _ =
  let function_tree =
    Node(
        Node(Leaf, (fun x -> x+1), Leaf),
         (fun x -> x*x),
         Node(Leaf, (fun x -> -x +x*x), Leaf)) in

  let composed = compose function_tree in

  assert_equal 0 @@ composed 0;
  assert_equal 12 @@ composed 1

let public_depth _ =
  let treea = Node(Node(Leaf, 1, Leaf), 2, Node(Leaf, 3, Leaf)) in
  let treeb = Node(Node(Leaf, 1, Leaf), 2, Node(Node(Leaf, 3, Leaf), 4, Leaf)) in

  assert_equal 2 @@ depth treea;
  assert_equal 3 @@ depth treeb

let public_trim _ =
  let treea = Node(Node(Leaf, 1, Leaf), 2, Node(Leaf, 3, Leaf)) in
  
  assert_equal
    ~printer: int_tree_printer
    (Node(Leaf, 2, Leaf)) @@
    trim treea 1

let public_tree_from_pre_in _ =
  let treea = Node(Node(Leaf, 1, Leaf), 2, Node(Leaf, 3, Leaf)) in
  let treeb = Node(Node(Leaf, 1, Leaf), 2, Node(Node(Leaf, 3, Leaf), 4, Leaf)) in

  assert_equal
    treea @@
    from_pre_in [2; 1; 3] [1; 2; 3];
  assert_equal
    treeb @@
    from_pre_in [2; 1; 4; 3] [1; 2; 3; 4]

let suite =
  "public" >::: [
    "public_insert1" >:: public_insert1;
    "public_remove1" >:: public_remove1;
    "public_remove2" >:: public_remove2;
    "public_query1" >:: public_query1;
    "public_query2" >:: public_query2;
    "public_sort" >:: public_sort;
    "public_queryBy" >:: public_queryby;
    "public_update" >:: public_update;
    "public_deleteAll" >:: public_deleteall;
    "public_tree_fold" >:: public_tree_fold;
    "public_map" >:: public_map;
    "public_mirror" >:: public_mirror;
    "public_in_order" >:: public_in_order;
    "public_pre_order" >:: public_pre_order;
    "public_compose" >:: public_compose;
    "public_depth" >:: public_depth;
    "public_trim" >:: public_trim;
    "public_from_pre_in" >:: public_tree_from_pre_in
    ]

let _ = run_test_tt_main suite
