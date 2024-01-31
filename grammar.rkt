#lang racket

(require (lib "eopl.ss" "eopl"))

(provide (all-defined-out))

; Program → Statements EOF
(define-datatype program program?
  (a-program
   (stats statements?)))
   
  

; Statements → Statement ';' | Statements Statement ';'
(define-datatype statements statements?
  (a-statement
   (stat statement?))
   
  (cum-statements
   (stats statements?)
   (stat statement?)))
   
  

; Statement → Compound_stmt | Simple_stmt
(define-datatype statement statement?
  (a-compound-stmt
   (cmp-stmt compound-stmt?))
   
  (a-simple-stmt
   (sim-stmt simple-stmt?)))
   
  

; Simple_stmt → Assignment | Global_stmt | Return_stmt
; Simple_stmt → 'pass' | 'break' | 'continue'
; Simple_stmt → 'print' '()' | 'print' '('Arguments')'
(define-datatype simple-stmt simple-stmt?
  (a-assign-stmt
   (assign assignment-stmt?))
   
  (glob-stmt
   (glob global-stmt?))
   
  (ret-stmt
   (ret return-stmt?))
   
  (pass-stmt)
  (break-stmt)
  (continue-stmt)
  (simple-print-stmt)
  (a-print-stmt
   (pstmt print-stmt?)))
   
(define-datatype print-stmt print-stmt?
 (a-print 
  (args arguments?)))

; Compound_stmt → Function_def | If_stmt | For_stmt
(define-datatype compound-stmt compound-stmt?
  (cmp-function-def
   (func-def function-def?))
   
  (cmp-if-stmt
   (if-stmt if-stmt?))
   
  (cmp-for-stmt
   (for-stmt for-stmt?)))
   
  

; Assignment → ID '=' Expression
(define-datatype assignment-stmt assignment-stmt?
  (a-assign
   (identifier symbol?)
   (expr expression?)))
   
  

; Return_stmt → 'return' | 'return' Expression
(define-datatype return-stmt return-stmt?
  (return-void-stmt)
  (return-exp-stmt
   (expr expression?)))
   
  

; Global_stmt → 'global' ID
(define-datatype global-stmt global-stmt?
  (a-global-stmt
   (identifier symbol?)))
   
  

; Function_def → 'def' ID '(' Params ')' ':' Statements | 'def' ID '():' Statements
(define-datatype function-def function-def?
  (func-def-with-params
   (identifier symbol?)
   (params params?)
   (stats statements?))
   
  (func-def-no-params
   (identifier symbol?)
   (stats statements?)))
   
  

; Params → Param_with_default | Params ',' Param_with_default
(define-datatype params params?
  (empty-param
   (param param?))
  (func-params 
   (param param?) 
   (rest-params params?)))
   
  

; Param_with_default → ID '=' Expression
(define-datatype param param?
  (with_default
   (identifier string?)
   (expr expression?)))
   
  

; If_stmt → 'if' Expression ':' Statements Else_block
(define-datatype if-stmt if-stmt?
  (a-if-stmt
   (cond-expr expression?)
   (if-stats statements?)
   (else-block else-block?)))
   
  

; Else_block → 'else' ':' Statements
(define-datatype else-block else-block?
  (a-else-block
   (else-stats statements?)))
   
  

; For_stmt → 'for' ID 'in' Expression ':' Statements
(define-datatype for-stmt for-stmt?
  (a-for-stmt
   (iter symbol?)
   (expr expression?)
   (stats statements?)))
   
  

; Expression → Disjunction
(define-datatype expression expression?
  (disjunct-expression
   (disjunc disjunction?)))
   
  

; Disjunction → Conjunction | Disjunction 'or' Conjunction
(define-datatype disjunction disjunction?
  (a-disjunction
   (conjunc conjunction?))
   
  (cum-disjunction
   (disjunc disjunction?)
   (conjunc conjunction?)))
   
  

; Conjunction → Inversion | Conjunction 'and' Inversion
(define-datatype conjunction conjunction?
  (a-conjunction
   (invers inversion?))
   
  (cum-conjunction
   (conjunc conjunction?)
   (invers inversion?)))
   
  

; Inversion → 'not' Inversion | Comparison
(define-datatype inversion inversion?
  (not-of-inversion
   (invers inversion?))
   
  (a-comparison
   (comparsion comparison?)))
   
  

; Comparison → Eq_Sum | Lt_Sum | Gt_Sum | Sum
(define-datatype comparison comparison?
  (eq-comp
   (eq-sum eq-sum?))
   
  (lt-comp
   (lt-sum lt-sum?))
   
  (gt-comp
   (gt-sum gt-sum?))
   
  (sum-comp
   (sum sum?)))
   
  

; Eq_Sum → Sum '==' Sum
(define-datatype eq-sum eq-sum?
  (a-eq-sum
   (sum1 sum?)
   (sum2 sum?)))
   
  

; Lt_Sum → sum '<' Sum
(define-datatype lt-sum lt-sum?
  (a-lt-sum
   (sum1 sum?)
   (sum2 sum?)))
   
  

; Gt_Sum → sum '>' Sum
(define-datatype gt-sum gt-sum?
  (a-gt-sum
   (sum1 sum?)
   (sum2 sum?)))
   
  

; Sum → Sum '+' Term | Sum '-' Term | Term
(define-datatype sum sum?
  (plus-sum
   (sum sum?)
   (term term?))
   
  (minus-sum
   (sum sum?)
   (term term?))
   
  (term-sum
   (term term?)))
   
  

; Term → Term '∗' Factor | Term '/' F actor | Factor
(define-datatype term term?
  (mult-term
   (term term?)
   (factor factor?))
   
  (div-term
   (term term?)
   (factor factor?))
   
  (factor-term
   (factor factor?)))
   
  

; Factor → '+' Power | '-' Power | Power
(define-datatype factor factor?
  (pos-factor
   (power power?))
   
  (neg-factor
   (power power?))
   
  (power-factor
   (power power?)))
   
  

; Power → Atom '∗ ∗' Factor | Primary
(define-datatype power power?
  (atom-powered
   (atom atom?)
   (factor factor?))
   
  (a-primary
   (primary primary?)))
   
  

; Primary → Atom | Primary '[' Expression ']' | Primary '()' 
; Primary → Primary '(' Arguments ')'
(define-datatype primary primary?
  (a-atom
   (atom atom?))
   
  (arr-access
   (primary primary?)
   (expr expression?))
   
  (func-call-no-arg
   (primary primary?))
   
  (func-call-with-args
   (primary primary?)
   (arguments arguments?)))
   
  

; Arguments → Expression | Arguments ',' Expression
(define-datatype arguments arguments? 
  (arg-expression
   (expr expression?))
   
  (args-expression
   (args arguments?)
   (expr expression?)))
   
  

(define-datatype none none?
  (none-stmt))
  

; Atom → ID | 'True' | 'False' | 'None' | NUMBER | List
(define-datatype atom atom?
  (a-id
   (identifier symbol?))
   
  (a-bool
   (bool boolean?))
   
  (a-none
   (none none?))
   
  (a-num
   (num number?))
   
  (a-list
   (py-list python-list?)))
   
  

; List → '[' Expressions ']' | '[]'
(define-datatype python-list python-list?
  (filled-list
   (exprs expressions?))
   
  (empty-list))
  

; Expressions → Expressions ',' Expression | Expression
(define-datatype expressions expressions?
  (cum-expression
   (exprs expressions?)
   (expr expression?))
   
  (a-expression
   (expr expression?)))
   
  