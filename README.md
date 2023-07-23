# fsmath

F# implementation of parser for simple mathematical expressions

Implementation is zealously immutable and stateless. No mutable variables and no type/module fields, aside from constants. No types except for discriminated unions, and no inheritence.

## Usage

```
fsmath <expression>
```

Examples:
```
$ fsmath 3 + 4
7

$ fsmath '-10 * -10'
100

$ fsmath -10 x -10
100

$ fsmath 3 + 4 + 2 + 1
10

$ fsmath '-(3 + 4) * 100'
-700

$ fsmath '-/(3 ^ 2 + 4 ^ 2)'
5
```
