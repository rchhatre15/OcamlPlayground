# Binary Trees and Database Implementation

This project implements two main components: a generic binary tree structure with various operations and a simple database system for managing person records. The implementation focuses on functional programming concepts in OCaml, particularly using tree folding for tree operations.

## Part 1: Binary Trees

### Features

#### Tree Operations (Using tree_fold)
- `map`: Apply a function to each node value
- `mirror`: Create a mirror image of the tree
- `in_order`: Generate in-order traversal list
- `pre_order`: Generate pre-order traversal list
- `compose`: Compose functions represented in a function tree
- `depth`: Calculate the depth of the tree
- `trim`: Trim the tree to a specified depth

#### Tree Construction
- `tree_init`: Initialize a tree using a generator function
- `from_pre_in`: Construct a tree from pre-order and in-order traversals

### Implementation Details

All tree operations are implemented using `tree_fold`, demonstrating the power and flexibility of fold operations. Key implementations include:

```ocaml
type 'a tree =
  | Node of 'a tree * 'a * 'a tree
  | Leaf

let tree_fold f init tree = match tree with
  | Leaf -> init
  | Node(l, x, r) -> 
      let left = tree_fold f init l in 
      let right = tree_fold f init r in 
      f left x right
```

## Part 2: Database System

### Features

#### Database Operations
- Insert records
- Remove records by name
- Query records based on conditions
- Sort records using comparators
- Map operations over records

#### Query Conditions
- Name matching
- Age comparison
- Hobbies filtering
- Logical operations (And, Or, Not)
- Conditional branching (If)

### Data Structures

```ocaml
type person = {
  name: string;
  age: int;
  hobbies: string list
}

type condition =
  | True
  | Name of (string -> bool)
  | Age of (int -> bool)
  | Hobbies of (string list -> bool)
  | And of condition * condition
  | Or of condition * condition
  | Not of condition
  | If of condition * condition * condition
```

## Usage Examples

### Tree Operations
```ocaml
(* Create and manipulate a binary tree *)
let tree = Node(Node(Leaf, 1, Leaf), 2, Node(Leaf, 3, Leaf))
let mirrored = mirror tree  (* Node(Node(Leaf, 3, Leaf), 2, Node(Leaf, 1, Leaf)) *)
let traversal = in_order tree  (* [1; 2; 3] *)
```

### Database Operations
```ocaml
(* Create and query a database *)
let person = {name="Alice"; age=23; hobbies=["Skiing"]}
let db = insert person newDatabase
let condition = Age (fun age -> age > 20)
let results = query condition db
```

## Building and Testing

To build the project:
```bash
dune build
```

To run the tests:
```bash
dune test
```

## Implementation Notes

### Tree Operations
- All tree operations use `tree_fold` for consistency and to demonstrate functional programming concepts
- The implementation handles edge cases (empty trees, single nodes)
- Tree construction from traversals uses efficient splitting algorithms

### Database System
- Implements a flexible query system using composable conditions
- Maintains immutability principles
- Provides comprehensive filtering and sorting capabilities

## Contributing

Feel free to submit issues and enhancement requests.

## License

This project is part of an academic exercise and is provided as-is.
