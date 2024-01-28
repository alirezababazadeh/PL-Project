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
   (stat statement?))
  (cum-statements
   (stats statements?)
   (stat statement?))
  )

; Statement → Compound_stmt | Simple_stmt
(define-datatype statement statement?
  (a-compound-stmt
   (cmp-stmt compound-stmt?))
  (a-simple-stmt
   (sim-stmt simple-stmt?))
  )

; Simple_stmt → Assignment | Global_stmt | Return_stmt
; Simple_stmt → 'pass' | 'break' | 'continue'
; Simple_stmt → 'print' '()' | 'print' '('Arguments')'
(define-datatype simple-stmt simple-stmt?
  (assign-stmt
   (assign assign-stmt?))
  (glob-stmt
   (glob global-stmt?))
  (ret-stmt
   (ret return-stmt?))
  (pass-stmt)
  (break-stmt)
  (continue-stmt)
  (simple-print-stmt)
  (print-stmt
   (args arguments?))
  )

; Compound_stmt → Function_def | If_stmt | For_stmt
(define-datatype compound-stmt compound-stmt?
  (a-function-def
   (func-def function-def?))
  (a-if-stmt
   (if-stmt if-stmt?))
  (a-for-stmt
   (for-stmt for-stmt?))
  )

