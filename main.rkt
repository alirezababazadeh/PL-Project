#lang racket

(require (lib "eopl.ss" "eopl"))
(require "parse.rkt")

(define run
  (lambda (string)
    (value-of-program (evaluate string))
    )
  )

(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (stats)
                 (value-of stats (init-env))))
    )
  )

(define-datatype program program?
  (a-program
   (stats statements?)
   )
  )

(define-datatype statements statements?
  (a-statement
   (stat statement?))
  (cum-statements
   (stats statements?)
   (stat statement?))
  )

(define-datatype statement statement?
  (a-compound-stmt
   (cmp-stmt compound-stmt?))
  (a-simple-stmt
   (sim-stmt simple-stmt?))
  )

(define-datatype simple-stmt simple-stmt?
  (assign
   (a assignment?))
  (glob-stmt
   (a global-stmt?))
  (ret-stmt
   (a return-stmt?))
  (pass-stmt)
  (break-stmt)
  (continue-stmt)
  (simple-print-stmt)
  (print-stmt
   (args arguments?))
  )

(define-datatype compound-stmt compound-stmt?
  (a-function-def
   (a function-def?))
  (a-if-stmt
   (a if-stmt?))
  (a-for-stmt
   (a for-stmt?))
  )

