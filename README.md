# simpleLisp

A boring Lisp implementation as an exercise.

## How to use
```
stack exec simpleLisp (add 1 (add 2 3))
````

## TODO
- Implement primitive functions:
  + quote
  + cons
  + define
  + lambda
  + cond
  + label
- Use State or Reader for user defined functions
- Improve errors
- Testing
  + Create Hspec parse failure tests
  + QuickCheck Arbitary Instance
  + Test parsing by generating random ASTs and parse their show value
- Improve REPL
  + Allow for deleting characters before submitting
  + Use MonadState to store command history with up/down to browse. For fun use a zipper.
- Replace Term with a GADT and Existentials
