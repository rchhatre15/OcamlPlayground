# NFA Regular Expression Engine

This project implements a Non-deterministic Finite Automaton (NFA) based regular expression engine in OCaml. It provides functionality to convert regular expressions to NFAs, perform NFA operations, and test string acceptance.

## Features

- Regular Expression to NFA Conversion
- NFA Operations:
  - Epsilon Closure
  - State Transitions
  - String Acceptance Testing
  - NFA to DFA Conversion
- Support for Basic Regex Operations:
  - Concatenation
  - Union (|)
  - Kleene Star (*)
  - Empty String (E)
  - Character Literals

## Project Structure

```
nfa-regex-project/
├── src/
│   ├── nfa.ml       # Core NFA implementation
│   ├── regexp.ml    # Regular expression parsing and conversion
│   └── sets.ml      # Set operations utility
├── test/
│   ├── public/      # Public test cases
│   ├── student/     # Student test cases
│   └── pbt/         # Property-based tests
└── bin/
    └── viz.ml       # Visualization utilities
```

## Implementation Details

### NFA Implementation (`nfa.ml`)
- Efficient epsilon closure computation using iterative approach
- State transition management
- String acceptance testing
- NFA to DFA conversion using subset construction

### Regular Expression Engine (`regexp.ml`)
- Parsing of regular expressions
- Conversion of regular expressions to NFAs
- Support for basic regex operations (union, concatenation, Kleene star)

## Building and Testing

To build the project:
```bash
dune build
```

To run the tests:
```bash
dune test
```

## Usage Examples

```ocaml
open P3.Nfa
open P3.Regexp

(* Create and test a simple regular expression *)
let regex = string_to_regexp "a(b|c)*"
let nfa = regexp_to_nfa regex
let result = accept nfa "abcbc"  (* true *)
```

## Performance Considerations

- The implementation uses an iterative approach for epsilon closure computation to prevent stack overflow with complex patterns
- Efficient state management using sorted unique lists
- Optimized NFA construction for union and Kleene star operations

## Contributing

Feel free to submit issues and enhancement requests.

## License

This project is part of an academic exercise and is provided as-is.
