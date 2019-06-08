# simpleLisp

A small Lisp implementation inspired by John McCarthy's 1960 paper `Recursive Functions of Symbolic Expressions
and Their Computation by Machine, Part I`

## How to use
```
# Interactive Repl
simpleLisp -i

# Run a file from disk
simpleLisp -o /path/to/lisp/script

# Interpret a lisp expression
simpleLisp -e "Some quoted lisp expression"
```

## TODO
- Implement `let` using ReaderT
- Testing
  + QuickCheck Arbitary Instance
  + Test parsing by generating random ASTs and parse their show value
