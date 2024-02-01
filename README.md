# Python-Interperter

## Overview

Python-Interperter is a simple programming language implemented in Racket, designed for the Designing Programming Language course. It features lazy evaluation and provides a set of grammar rules for defining statements, assignments, functions, and expressions. The language supports compound statements, conditionals, loops, and basic arithmetic operations.

## Table of Contents

1. [Introduction](#introduction)
2. [Grammar](#grammar)
3. [Usage](#usage)
4. [Examples](#examples)

## Introduction

Python-Interperter is an interpreter developed as a part of the Designing Programming Language course. It is implemented in Racket and incorporates lazy evaluation for efficient computation.

## Grammar

The language follows the specified grammar rules:

```
1. Program → Statements EOF
2. Statements → Statement ‘; ‘ | Statements Statement ‘; ‘
3. Statement → Compound_stmt | Simple_stmt
4. Simple_stmt → Assignment | Global_stmt | Return_stmt | ‘pass‘ |‘break‘ | ‘continue‘ | ‘print‘ ‘()‘ | ‘print‘ ‘(‘Arguments‘)‘
5. Compound_stmt → Function_def | If_stmt | For_stmt
6. Assignment → ID ‘ = ‘ Expression
7. Return_stmt → ‘return‘ | ‘return‘ Expression
8. Global_stmt → ‘global‘ ID
9. Function_def → ‘def‘ ID ‘(‘ Params ‘)‘ ‘ : ‘ Statements | ‘def‘ ID ‘() : ‘ Statements
10. Params → Param_with_default | Params ‘, ‘ Param_with_default
11. Param_with_default → ID ‘ = ‘ Expression
12. If_stmt → ‘if‘ Expression ‘ : ‘ Statements Else_block
13. Else_block → ‘else‘ ‘ : ‘ Statements
14. For_stmt → ‘for‘ ID ‘in‘ Expression ‘ : ‘ Statements
15. Expression → Disjunction
16. Disjunction → Conjunction | Disjunction ‘or‘ Conjunction
17. Conjunction → Inversion | Conjunction ‘and‘ Inversion
18. Inversion → ‘not‘ Inversion | Comparison
19. Comparison → Eq_Sum| Lt_Sum | Gt_Sum | Sum
20. Eq_Sum → sum ‘ == ‘ Sum
21. Lt_Sum → sum ‘ < ‘ Sum
22. Gt_Sum → sum ‘ > ‘ Sum
23. Sum → Sum ‘ + ‘ Term | Sum ‘ − ‘ Term | Term
24. Term → Term ‘ ∗ ‘ Factor | Term ‘/‘ Factor | Factor
25. Factor → ‘ + ‘ Power | ‘ − ‘ Power | Power
26. Power → Atom ‘ ∗ ∗‘ Factor | Primary
27. Primary → Atom | Primary ‘[‘ Expression ‘]‘ | Primary ‘()‘ | Primary ‘(‘ Arguments ‘)‘
28. Arguments → Expression | Arguments ‘, ‘ Expression
29. Atom → ID | ‘True‘ | ‘False‘ | ‘None‘ | NUMBER | List
30. List → ‘[‘ Expressions ‘]‘ | ‘[]‘
31. Expressions → Expressions ‘, ‘ Expression | Expression
```

## Usage
To use this, follow these steps:

1. Install Racket: Racket Installation Guide
2. Clone the LazyEval repository.
3. Run your programs using the Racket interpreter.

## Examples
Here are some examples of LazyEval code:

```
# Simple Function Definition
def add(x, y): 
    return x + y;

# Conditional Statement
if x > 0:
    print("Positive");
else:
    print("Non-Positive");

# Looping Statement
for i in range(5):
    print(i);

# Lazy Evaluation
result = 1 + (print("This will be lazily evaluated") or 0);
```
