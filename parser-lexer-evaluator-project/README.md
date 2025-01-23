# MicroCaml Parser, Lexer, and Evaluator

A complete implementation of a small functional programming language (MicroCaml) including lexical analysis, parsing, and evaluation components. This project demonstrates the fundamental concepts of programming language implementation.

## Features

### Lexical Analysis
- Token recognition for:
  - Basic operators (+, -, *, /, >, <, =, etc.)
  - Keywords (let, rec, if, then, else, fun)
  - Identifiers and literals (integers, booleans, strings)
  - Comments and whitespace handling

### Parsing
- Recursive descent parser implementation
- Support for:
  - Let bindings (both recursive and non-recursive)
  - Function definitions and application
  - Conditional expressions (if-then-else)
  - Binary operations
  - Basic arithmetic and boolean operations

### Evaluation
- Dynamic evaluation of MicroCaml expressions
- Environment-based variable management
- Support for:
  - First-class functions with closures
  - Recursive function definitions
  - Let bindings with proper scoping
  - Binary operations with type checking

## Project Structure

```
parser-lexer-evaluator-project/
├── src/
│   ├── lexer.ml     # Lexical analysis implementation
│   ├── parser.ml    # Parsing implementation
│   ├── eval.ml      # Evaluation implementation
│   └── utils.ml     # Utility functions
├── test/
│   ├── public/      # Public test cases
│   └── student/     # Student test cases
└── bin/
    └── mutop.ml     # REPL implementation
```

## Implementation Details

### Lexer (`lexer.ml`)
- Converts input strings into tokens
- Handles nested comments and string literals
- Implements proper error reporting for invalid tokens

### Parser (`parser.ml`)
- Implements recursive descent parsing
- Builds abstract syntax tree (AST) from token stream
- Handles operator precedence and associativity

### Evaluator (`eval.ml`)
- Implements environment-based evaluation
- Supports recursive functions through temporary bindings
- Provides proper error handling for type mismatches and undefined variables

## Usage Examples

```ocaml
(* Define a recursive factorial function *)
let rec fact = fun x ->
  if x = 1 then 1
  else x * fact (x - 1);;

(* Use the factorial function *)
fact 5;;  (* Returns 120 *)

(* Let binding with function application *)
let x = 10 in
let f = fun y -> x + y in
f 20;;  (* Returns 30 *)
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

To start the REPL:
```bash
dune exec bin/mutop.exe
```

## Error Handling

The implementation includes comprehensive error handling for:
- Type errors in operations
- Undefined variables
- Division by zero
- Invalid syntax
- Lexical errors

## Contributing

Feel free to submit issues and enhancement requests.

## License

This project is part of an academic exercise and is provided as-is.
