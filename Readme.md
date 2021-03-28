# LOXL

# A Lox interpreter written in Common Lisp
An implementation of the Lox language from part II of "[Crafting Interpreters](https://craftinginterpreters.com)" written in Common Lisp

## Running the interpreter
Run `make` to build and executable called `loxl`.  Running `loxl` with a file path with run a *.lox file, or will run a repl with no arguments.

## Running the tests
Loxl tests against all of the *.lox files in the Crafting Interpreters github repo.  To run them, execute `(asdf:test-system 'loxl)`

## Challenges Implemented
* C-Style block comments
* String coercion during concatenation
* Comma and Ternary operators
* Accessing non-initialized variables throws an error
* Break keyword for while/for loops
* Anonymous functions

## Misc Additions
* Repl can handle multi-line input from the user
