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

1. Install Racket: [Racket Installation Guide](https://docs.racket-lang.org/pollen/Installation.html)
2. Clone this repository.
3. Run your programs using the Racket interpreter.

## Examples
Here are some examples:

```
# Simple Function Definition
def add(x = 0, y = 0): 
    return x + y;;

print(add(1, 2));
print(add());

# Conditional Statement
x = 1
if x > 0:
    print(x);
else:
    print(0);

# For Statement
def get_first_even_idx(l = []):
    j = 0;
    len = 0;
    for i in l:
        len = len + 1;
    ;
    
    for i in l:
        if i == 2 or i == 2 or i == 4 or i == 6 or i == 8:
            break;
        else:
            j = j + 1;
            continue;
        ;
    ;

    if j == len:
        j = -1;
    else:
        pass;
    ;

    return j;
;

l = [1, 5, 7, 9, 2, 1];
z = [1, 3, 5];

print(l);
print(get_first_even_idx(l));

print(z);
print(get_first_even_idx(z));

# Lazy Evaluation
x = 0;
def func():
    global x;
    x = 1;
;

x = 2;
print(x);
a = func();
print(x);
print(a);
print(x);
```
