# simpleLisp

A small Lisp implementation based on John McCarthy's 1960 paper `Recursive Functions of Symbolic Expressions
and Their Computation by Machine, Part I`

## How to use
```
stack exec simpleLisp (add 1 (add 2 3))
````

## TODO
- Implement primitive functions:
  + define
  + lambda
  + cond
  + label
- Use State or Reader for user defined functions
- Improve errors
- Testing
  + QuickCheck Arbitary Instance
  + Test parsing by generating random ASTs and parse their show value
- Replace Term with a GADT and Existentials
