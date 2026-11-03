# MiniZinc-Lite Grammar (BNF)
-- This BNF file structure/format was suggested by chatgpt; reviewed and fine-tuned by claude. I've also reviewed tje final work, and contents are from myself.

## Notation

Standard BNF:

- `<name>` is a non-terminal, `"text"` is a terminal (token).
- `::=` defines a rule, `|` separates alternatives.
- `ε` is the empty string.

Repetition and optional parts are written with recursion and `ε` only, so
the grammar is plain BNF (no EBNF `*`, `+` or `[ ]`).

The grammar describes **syntax only**. Rules that depend on what has been
declared (scope, duplicates, values of constants) are checked by the parser
after a construct has been read; they are listed in
[Semantic rules](#3-semantic-rules-checked-by-the-parser-not-by-the-grammar).

## 1. Lexical structure

```bnf
<identifier>  ::= <letter> <ident-rest>     ; except the reserved words below
<ident-rest>  ::= ε | <ident-char> <ident-rest>
<ident-char>  ::= <letter> | <digit> | "_"

<natural>     ::= <digit> | <digit> <natural>

<digit>       ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
<letter>      ::= "a" | ... | "z" | "A" | ... | "Z"     ; and other letters
```

- `<letter>` is any character accepted by `Data.Char.isAlpha`; `<digit>` is
  `'0'`..`'9'` (`isDigit`). Integer literals have no sign: a leading `-` is
  always the unary minus operator (or the sign of a domain bound).
- **Whitespace** (any `isSpace` character, including newlines and tabs) may
  appear before the first token and between any two tokens. It is only
  required where two tokens would otherwise join into one word: `var 1..3`
  and `x mod 3` need it (`var1` is a single identifier), but
  `int:n=3;var 1..n:x;` does not.
- There is **no comment syntax**.
- **Keywords** are read as a whole word, so `summary`, `minimum` and
  `model` are ordinary identifiers, and `solvesatisfy` is a single
  identifier rather than `solve satisfy`.
- **Reserved words** cannot be used as identifiers:

  ```
  var  int  constraint  solve  array  of  in
  satisfy  maximize  minimize
  sum  min  max  all  any  allDifferent
  mod  abs
  ```

## 2. Grammar

### 2.1 Program

Declarations and constraints may appear in any order; the program ends with
exactly one solve directive, followed by the end of the input.

```bnf
<program>        ::= <statements> <solve-item>

<statements>     ::= ε | <statement> <statements>

<statement>      ::= <constant-decl>
                   | <variable-decl>
                   | <array-decl>
                   | <constraint-item>
```

### 2.2 Declarations

```bnf
<constant-decl>  ::= "int" ":" <identifier> "=" <expr> ";"

<variable-decl>  ::= "var" <domain> ":" <identifier> ";"

<array-decl>     ::= "array" "[" <natural> ".." <bound> "]" "of"
                     "var" <domain> ":" <identifier> ";"
```

### 2.3 Domains

```bnf
<domain>         ::= <range> | <finite-set>

<range>          ::= <bound> ".." <bound>

<bound>          ::= <natural>
                   | "-" <natural>
                   | <identifier>
                   | "-" <identifier>

<finite-set>     ::= "{" <int-list> "}"

<int-list>       ::= <signed-int> | <signed-int> "," <int-list>

<signed-int>     ::= <natural> | "-" <natural>
```

The same `<domain>` rule is used for variable and array element domains and
for generator domains (`i in 1..n`, `i in {1, 3}`).

### 2.4 Constraints

```bnf
<constraint-item>  ::= "constraint" <constraint-body> ";"

<constraint-body>  ::= <boolean-macro> | <comparison>

<comparison>       ::= <expr> <compare-op> <expr>

<compare-op>       ::= "=" | "!=" | "<" | "<=" | ">" | ">="

<boolean-macro>    ::= "all" <condition-args>
                     | "any" <condition-args>
                     | "allDifferent" <expr-args>
```

There is no `==`: `x == 1` is a syntax error (after `x =`, the text `= 1` is
not an expression).

### 2.5 Macro arguments (literal, whole-array and generator forms)

Every macro accepts the same three forms. `all` and `any` take conditions
(comparisons); `sum`, `min`, `max` and `allDifferent` take expressions.

```bnf
<expr-args>       ::= "(" "[" <expr-list> "]" ")"
                    | "(" <identifier> ")"
                    | "(" <identifier> "in" <domain> ")" "(" <expr> ")"

<condition-args>  ::= "(" "[" <comparison-list> "]" ")"
                    | "(" <identifier> ")"
                    | "(" <identifier> "in" <domain> ")" "(" <comparison> ")"

<expr-list>       ::= <expr> | <expr> "," <expr-list>

<comparison-list> ::= <comparison> | <comparison> "," <comparison-list>
```

Lists are non-empty and have no trailing comma (`sum([])` and `sum([x,])`
are rejected). A macro with no brackets, such as `sum(x, y)`, is rejected:
`(x` is read as a whole array name and the `,` cannot follow it.

### 2.6 Arithmetic expressions

One non-terminal per precedence level, from lowest to highest binding:

```bnf
<expr>      ::= <term>
              | <expr> <add-op> <term>

<add-op>    ::= "+" | "-"

<term>      ::= <unary>
              | <term> <mul-op> <unary>

<mul-op>    ::= "*" | "/" | "mod"

<unary>     ::= "-" <unary>
              | <power>

<power>     ::= <atom>
              | <atom> "^" <unary>

<atom>      ::= <numeric-macro>
              | "abs" "(" <expr> ")"
              | <natural>
              | <identifier> "[" <expr> "]"
              | <identifier>
              | "(" <expr> ")"

<numeric-macro> ::= <numeric-name> <expr-args>

<numeric-name>  ::= "sum" | "min" | "max"
```

| Level (highest first) | Operators                | Associativity |
| --------------------- | ------------------------ | ------------- |
| 1                     | `^`                      | right         |
| 2                     | unary `-`                | prefix        |
| 3                     | `*`, `/`, `mod`          | left          |
| 4                     | `+`, binary `-`          | left          |

How the rules encode this:

- `<expr>` and `<term>` are **left-recursive**, so `10 - 3 - 2` is
  `(10 - 3) - 2` and `x * y mod 3` is `(x * y) mod 3`. (The parser reads
  them with `chainLeft`, which folds the operands from the left with
  `foldl`, since a combinator parser cannot be written left-recursively.)
- In `<power>` the exponent is a `<unary>`, which contains `<power>` again,
  so `2^3^2` is `2^(3^2)` (right associative).
- The operand of unary `-` is a whole `<power>`, so `-x^2` is `-(x^2)`.
- The operands of `*`, `/` and `mod` are `<unary>`s, so `-x mod 3` is
  `(-x) mod 3` and `2 * -3` is `2 * (-3)`.

Parse tree of `-x^2 + 1`. The `^` is inside the unary `-`, so `x^2` is
computed first:

```
<expr>
├── <expr> ─ <term> ─ <unary>
│                      ├── "-"
│                      └── <unary> ─ <power>
│                                    ├── <atom> ─ "x"
│                                    ├── "^"
│                                    └── <unary> ─ <power> ─ <atom> ─ "2"
├── "+"
└── <term> ─ <unary> ─ <power> ─ <atom> ─ "1"
```

### 2.7 Solve directive

```bnf
<solve-item>  ::= "solve" "satisfy" ";"
                | "solve" "minimize" <expr> ";"
                | "solve" "maximize" <expr> ";"
```

## 3. Semantic rules (checked by the parser, not by the grammar)

A context-free grammar cannot say "this name was declared earlier" or "this
range is not empty". These conditions are checked while parsing, using the
scope built from the statements read so far; a program that fits the
grammar but breaks one of them is rejected.

| Rule | Example that is rejected |
| ---- | ------------------------ |
| Every name is declared once (constants, variables and arrays share one namespace). | `var 1..3: x; var 1..5: x;` |
| A name is declared before it is used (in statement order). | `constraint x > 1; var 1..3: x;` |
| A constant's expression may only use earlier constants (no variables or arrays), and must evaluate (no division by zero). | `var 1..3: x; int: a = x + 1;`, `int: a = 5 / 0;` |
| A range bound that is an identifier must be a declared constant. | `var 1..n: x; int: n = 3;` |
| A range is non-empty: lower bound ≤ upper bound. | `var 5..1: x;` |
| A finite set has no repeated values. | `var {1, 2, 1}: x;` |
| An array's lower index is the literal `1` and its length is at least 1. | `array[0..3] ...`, `int: n = 0; array[1..n] ...` |
| `q[...]` and the whole-array form `(q)` must name an array; a plain name must not be an array. | `var 1..3: x; constraint x[1] = 2;`, `array[1..3] of var 1..3: q; constraint q = 2;` |
| A generator variable is local to its body; inside the body it shadows an outer name with the same spelling. | `constraint sum(i in 1..3)(q[i]) = 6; constraint i = 2;` |
| Nothing may follow the solve directive. | `solve satisfy; solve satisfy;` |

Two related decisions are deliberately left to the solver:

- An array index that is out of bounds (`q[5]` for an array of length 3) is
  accepted by the parser; the specification treats it as an invalid
  *assignment*, so the solver rejects every assignment that uses it.
- `all(q)` / `any(q)` (a whole integer array where conditions are expected)
  fits the grammar; the solver treats it as invalid, so such a model has no
  solution.

## 4. Meaning of the operators

| Construct | Meaning |
| --------- | ------- |
| `a / b`   | Integer division rounding down (Haskell `div`): `-7 / 3 = -3`. |
| `a mod b` | Haskell `mod`, the remainder of `/`, with the sign of `b`: `-7 mod 3 = 2`, `7 mod -3 = -2`. (Real MiniZinc truncates towards zero instead.) |
| `abs(a)`  | Absolute value. |
| `a ^ b`   | Power; a negative exponent is invalid. |

Division or `mod` by zero, a negative exponent, an out-of-bounds index, or a
`/` / `mod` result that does not fit in a 64-bit `Int` (such as
`minBound / -1`) makes the current assignment invalid; it never stops the
solver.

`+`, `-`, `*`, `^` and `abs` use 64-bit `Int` arithmetic and wrap around on
overflow, as in Haskell. Only `/` and `mod` check the range, because
`minBound / -1` would otherwise throw an exception.

## 5. Complete example

The provided N-Queens model, with the rule used for each line:

```minizinc
int: n = 8;                                     % <constant-decl>
array[1..n] of var 1..n: q;                     % <array-decl>
constraint allDifferent(q);                     % whole-array <expr-args>
constraint allDifferent(i in 1..n)(q[i] + i);   % generator <expr-args>
constraint allDifferent(i in 1..n)(q[i] - i);
solve minimize q[1];                            % <solve-item>
```

(`%` comments are shown here for explanation only; the language itself has
no comment syntax.)

## 6. Correspondence with the parser

| Grammar rule        | Parser function(s) in `MiniZincParser.hs` |
| ------------------- | ----------------------------------------- |
| `<program>`, `<statements>` | `miniZincParser`, `program` |
| `<constant-decl>`   | `constantDeclaration` |
| `<variable-decl>`   | `variableDeclaration`, `varBody` |
| `<array-decl>`      | `arrayDeclaration`, `arrayLength`, `varBody` |
| `<domain>`, `<range>`, `<bound>`, `<finite-set>` | `domainParser`, `rangeParser`, `domainBound`, `finiteSetParser` |
| `<constraint-item>`, `<constraint-body>` | `constraintParser`, `constraintBody` |
| `<comparison>`, `<compare-op>` | `comparisonParser`, `comparisonOperator` |
| `<boolean-macro>`, `<numeric-macro>` | `booleanMacroParser`, `numericMacroParser` |
| `<expr-args>`, `<condition-args>` | `macroArgs`, `macroInput`, `generatorParser`, `listParser` |
| `<expr>` / `<term>` | `additionParser` / `multiplicationParser` (via `chainLeft`) |
| `<unary>`, `<power>`, `<atom>` | `unaryParser`, `powerParser`, `atomParser`, `absParser`, `arrayAccessParser` |
| `<solve-item>`      | `solveDirectiveParser`, `solveGoalParser` |
| keywords, reserved words | `keyword`, `identifier`, `reservedWords` |
| semantic rules (section 3) | `declare`, `addName`, `checkReferences`, `rangeParser`, `finiteSetParser`, `arrayLength`, `eof` |
