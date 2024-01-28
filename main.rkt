#lang racket

(require (lib "eopl.ss" "eopl"))
(require "parse.rkt")

(define evaluate
  (lambda (input-string)
    (value-of-program (scan&parse input-string))
    )
  )

(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (stats)
                (value-of stats (init-env))))
    )
  )

; Program → Statements EOF
(define-datatype program program?
  (a-program
   (stats statements?)
   )
  )

; Statements → Statement ';' | Statements Statement ';'
(define-datatype statements statements?
  (a-statement
   (stat statement?)
   )
  (cum-statements
   (stats statements?)
   (stat statement?)
   )
  )

; Statement → Compound_stmt | Simple_stmt
(define-datatype statement statement?
  (a-compound-stmt
   (cmp-stmt compound-stmt?)
   )
  (a-simple-stmt
   (sim-stmt simple-stmt?)
   )
  )

; Simple_stmt → Assignment | Global_stmt | Return_stmt
; Simple_stmt → 'pass' | 'break' | 'continue'
; Simple_stmt → 'print' '()' | 'print' '('Arguments')'
(define-datatype simple-stmt simple-stmt?
  (assign-stmt
   (assign assignment-stmt?)
   )
  (glob-stmt
   (glob global-stmt?)
   )
  (ret-stmt
   (ret return-stmt?)
   )
  (pass-stmt)
  (break-stmt)
  (continue-stmt)
  (simple-print-stmt)
  (print-stmt
   (args arguments?)
   )
  )

; Compound_stmt → Function_def | If_stmt | For_stmt
(define-datatype compound-stmt compound-stmt?
  (cmp-function-def
   (func-def function-def?)
   )
  (cmp-if-stmt
   (if-stmt if-stmt?)
   )
  (cmp-for-stmt
   (for-stmt for-stmt?)
   )
  )

; Assignment → ID '=' Expression
(define-datatype assignment-stmt assignment-stmt?
  (a-assign-stmt
   (identifier symbol?)
   (expr expression?)
   )
  )

; Return_stmt → 'return' | 'return' Expression
(define-datatype return-stmt return-stmt?
  (return-void-stmt)
  (return-exp-stmt
   (expr expression?)
   )
  )

; Global_stmt → 'global' ID
(define-datatype global-stmt global-stmt?
  (a-global-stmt
   (identifier symbol?)
   )
  )

; Function_def → 'def' ID '(' Params ')' ':' Statements | 'def' ID '():' Statements
(define-datatype function-def function-def?
  (func-def-with-params
   (identifier symbol?)
   (params params?)
   (stats statements?)
   )
  (func-def-no-params
   (identifier symbol?)
   (stats statements?)
   )
  )

; Params → Param_with_default | Params ',' Param_with_default
(define-datatype params params?
  (empty-param)
  (func-params 
   (param param?) 
   (rest-params params?)
   )
  )

; Param_with_default → ID '=' Expression
(define-datatype param param?
  (with_default
   (identifier string?)
   (expr expression?)
   )
  )

; If_stmt → 'if' Expression ':' Statements Else_block
(define-datatype if-stmt if-stmt?
  (a-if-stmt
   (cond-expr expression?)
   (if-stats statements?)
   (else-block else-block?)
   )
  )

; Else_block → 'else' ':' Statements
(define-datatype else-block else-block?
  (a-else-block
   (else-stats statements?)
   )
  )
