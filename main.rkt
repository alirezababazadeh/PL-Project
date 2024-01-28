#lang racket

(require (lib "eopl.ss" "eopl"))
(require "parse.rkt"
         "grammar.rkt")

(define evaluate
  (lambda (input-string)
    (value-of-program (scan&parse input-string))
    )
  )

(define-datatype answer answer?
  (a-ans
   (value expval?)
   (msg symbol?)
   (env environment?)))


(define answer-val
  (lambda (ans)
    (cases answer ans
      (a-ans (val msg env) val))))

(define extract-env
  (lambda (ans)
    (cases answer ans
      (a-ans (val msg env) env))))

(define ret-ans?
  (lambda (ans)
    (cases answer ans
      (a-ans (val msg env) (eqv? msg 'return)))))

(define continue-ans?
  (lambda (ans)
    (cases answer ans
      (a-ans (val msg env) (eqv? msg 'continue)))))

(define break-ans?
  (lambda (ans)
    (cases answer ans
      (a-ans (val msg env) (eqv? msg 'break)))))

(define-datatype thunk thunk?
  (a-thunk
   (exp expression?)
   (env environment?)))

; Program
(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (stats)
                 (value-of-stats stats (init-env))))
    )
  )

; extract-env
; break-ans? - ret-ans? - continue-ans?
; Statements
(define value-of-stats
  (lambda (stats env)
    (cases statements stats
      (a-statement (stat)
                   (value-of-stmt stat env)
                   )
      (cum-statements (stats stat)
                      (let ((answer-stats (value-of-statements stats env)))
                        (if (not (or (ret-ans? answer-stats) (break-ans? answer-stats) (continue-ans? answer-stats)))
                            (value-of-stmt stat (extract-env answer-stats))
                            answer-stats)
                        )
                      )
      )
    )
  )

; Statement
(define value-of-stmt
  (lambda (stat env)
    (cases statement stat
      (a-compound-stmt (cmp-stat) (value-of-compound-stmt cmp-stat env))
      (a-simple-stmt (sim-stmt) (value-of-simple-stmt sim-stmt env))
      )))

; Simple-stmt
(define value-of-simple-stmt
  (lambda (stat env)
    (cases simple-stmt stat
      (assign-stmt (assign) (value-of-assignment assign env))
      (glob-stmt (glob) (value-of-global-stmt glob env))
      (ret-stmt (ret) (value-of-return ret env))
      (pass-stmt () (a-ans (a-none) '- env))
      (beak-stmt () (a-ans (a-none) 'break env))
      (continue-stmt () (a-ans (a-none) 'continue env))
      )
    ))

; Assignment-stmt
(define value-of-assignment
  (lambda (stat env)
    (cases assignment-stmt stat
      (a-assign-stmt (identifier expr) (a-ans (a-none) '- (extend-env env identifier (a-thunk expr (copy-of-env env)))))
      )
    )
  )

; Return-stmt
(define value-of-return
  (lambda (stat env)
    (cases return-stmt stat
      (return-void-stmt () (a-ans (a-none) 'return env))
      (return-exp-stmt (expr) (let
                                  ((resp (value-of-expression expr env)))
                                (a-ans (ans-val resp) 'return (extract-env resp))))
      )
    )
  )