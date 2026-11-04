---
title: Assignment 2 - Minzinc-Lite
author: FIT2102 Programming Paradigms
margin: 1inch
---

Please do not change the names of the functions defined in the Assignment.hs file. You may (and are highly encouraged) to implement your parsers **alongside** these pre-defined functions.

## Running the Code

```
$ stack test
```

This will generate the output files using the sample input MiniZinc files, by running your code.

All example MiniZinc files are stored within `examples/inputs` and the output of your parser will be saved in `examples/output`.

## Running the Interactive Page

In the Haskell folder run:

```
$ stack run
```

In a separate terminal, in the javascript folder run:

```
$ npm i
$ npm run dev
```

You can type in to the LHS of the webpage and inspect the generated solution.

## Assignment 2 Extensions (Part E)

The following features are implemented in addition to the required
MiniZinc-Lite functionality. They are listed here to make the scope of
each extension clear to the marker.

### E1 — Bounds Reasoning (Solver Optimisation)

**Category:** Optimize solving of problems.

**Implementation:** `Haskell/src/MiniZincBounds.hs`, integrated through
`Haskell/src/MiniZincSolver.hs`.

The normal solver evaluates constraints incrementally and rejects constraints
that are already false. This compulsory behaviour is retained. **The extra
extension** additionally computes conservative integer intervals for
expressions involving unassigned variables.

- Assigned variables are represented by exact values; unassigned variables
  are represented by intervals covering their domains.
- If the intervals prove that a comparison cannot be true under *any*
  completion of the current assignment, the search branch is pruned.
- `sum`, `min`, `max`, arrays and generator expressions are handled by the
  bounds implementation. It also handles arithmetic including `mod` and
  `abs` where a safe bound can be established.
- When sound bounds are unavailable (for example, unsafe division or
  potential integer overflow), bounds reasoning declines to prune.
- Early duplicate detection for partial `allDifferent` constraints is
  **required Part D functionality**, not claimed as an extension.

**Example:**

```minizinc
array[1..3] of var 1..3: q;
constraint sum(q) = 20;
solve satisfy;
```

Each array element is at most 3, so the total can be at most 9. The bounds
extension can reject the impossible comparison without enumerating every
complete assignment.

### E2 — MiniZinc-Lite BNF Grammar

**Category:** Create a BNF for the MiniZinc language (up to 2 bonus marks).

**Document:** `docs/MiniZincLite-BNF.md`

The document describes the implemented language syntax using BNF, including
declarations, domains, expression precedence, constraint macros, arrays,
generator forms and solve directives. It separately explains semantic
validation rules that are not captured by context-free grammar alone.

### E3 — Additional Language Capabilities: `mod` and `abs`

**Category:** Add more capability to your language.

**Implementation:** `Haskell/src/MiniZincADT.hs`,
`Haskell/src/MiniZincParser.hs`, `Haskell/src/MiniZincEval.hs`,
`Haskell/src/MiniZincBounds.hs`.

Two additional arithmetic expression forms are supported:

- `a mod b` computes integer remainder using Haskell `mod` semantics.
  It has the same precedence level as `*` and `/`; division by zero
  invalidates that candidate assignment rather than crashing.
- `abs(expression)` computes the absolute value of an expression.

These can be used inside constraints, macro expressions, generators and
solve objectives.

**Example:**

```minizinc
var -5..5: x;
constraint abs(x) = 3;
constraint x mod 2 = 1;
solve minimize x;
```

The valid and optimal assignment is `x = -3`.

## Part C: Save Solution

Click **Save Solution** after entering a model. The backend writes
`solution_<currentTime>.out` (using the provided `getTime`) in the
directory where `stack run` was started, usually `Haskell/`.

## AI Declaration

ChatGPT(https://chatgpt.com/) and Claude(https://claude.com/) were used throughout this assignment: for design suggestions, writing and refactoring parts of the Haskell and TypeScript code, refining comments, and organising the README and BNF. I reviewed and verified the final content.
