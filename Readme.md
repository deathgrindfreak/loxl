# LOXL

# A Lox interpreter written in Common Lisp
An implementation of the Lox language from part II of "[Crafting Interpreters](https://craftinginterpreters.com)" written in Common Lisp

## Running the interpreter
Right now, I haven't created a way to generate an executable, but loading the system with ASDF and running `main` from the `loxl` package with a file argument will run the script, or without an argument will start a Lox repl.

## Challenges Implemented
* C-Style block comments
* String coercion during concatenation
* Comma and Ternary operators
* Accessing non-initialized variables throws an error

## Misc Additions
* Repl can handle multi-line input from the user
