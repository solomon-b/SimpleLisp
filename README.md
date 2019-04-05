# simpleLisp

A boring Lisp implementation as an exercise.

## How to use
```
stack exec simpleLisp (add 1 (add 2 3))
````

## TODO
- Implement primitive functions
- Err on unbound Atoms
- Fix dotlist parsing
- Replicate mit-scheme list semantics: eg., (1 1) should err for attempting to call `1`
- Improve errors
- Use State or Reader for user defined functions
- QuickCheck tests based on parsing the show values of arbitrary Cons trees.
- A REPL
- Replace Term with a GADT and Existentials
