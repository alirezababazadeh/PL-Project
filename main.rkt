#lang racket

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

; add statements and other stuff
