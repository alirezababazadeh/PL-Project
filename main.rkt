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
   (sc scope?)))


(define answer-val
  (lambda (ans)
    (cases answer ans
      (a-ans (val msg sc) val))))

(define extract-sc
  (lambda (ans)
    (cases answer ans
      (a-ans (val msg sc) sc))))

(define ret-ans?
  (lambda (ans)
    (cases answer ans
      (a-ans (val msg sc) (eqv? msg 'return)))))

(define continue-ans?
  (lambda (ans)
    (cases answer ans
      (a-ans (val msg sc) (eqv? msg 'continue)))))

(define break-ans?
  (lambda (ans)
    (cases answer ans
      (a-ans (val msg sc) (eqv? msg 'break)))))

(define-datatype thunk thunk?
  (a-thunk
   (exp expression?)
   (sc scope?)))

; Program
(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (stats)
                 (value-of-stats stats (new-global-scope))))
    )
  )

; Statements
(define value-of-stats
  (lambda (stats sc)
    (cases statements stats
      (a-statement (stat)
                   (value-of-stmt stat sc)
                   )
      (cum-statements (stats stat)
                      (let ((answer-stats (value-of-statements stats sc)))
                        (if (not (or (ret-ans? answer-stats) (break-ans? answer-stats) (continue-ans? answer-stats)))
                            (value-of-stmt stat (extract-sc answer-stats))
                            answer-stats)
                        )
                      )
      )
    )
  )

; Statement
(define value-of-stmt
  (lambda (stat sc)
    (cases statement stat
      (a-compound-stmt (cmp-stat) (value-of-compound-stmt cmp-stat sc))
      (a-simple-stmt (sim-stmt) (value-of-simple-stmt sim-stmt sc))
      )))

; Simple-stmt
(define value-of-simple-stmt
  (lambda (stat sc)
    (cases simple-stmt stat
      (assign-stmt (assign) (value-of-assignment assign sc))
      (glob-stmt (glob) (value-of-global-stmt glob sc))
      (ret-stmt (ret) (value-of-return ret sc))
      (pass-stmt () (a-ans (a-none) '- sc))
      (beak-stmt () (a-ans (a-none) 'break sc))
      (continue-stmt () (a-ans (a-none) 'continue sc))
      )
    ))

; Assignment-stmt
(define value-of-assignment
  (lambda (stat sc)
    (cases assignment-stmt stat
      (a-assign-stmt (identifier expr) (a-ans (a-none) '- (extend-sc sc identifier (a-thunk expr (copy-of-sc sc)))))
      )
    )
  )

; Return-stmt
(define value-of-return
  (lambda (stat sc)
    (cases return-stmt stat
      (return-void-stmt () (a-ans (a-none) 'return sc))
      (return-exp-stmt (expr) (let
                                  ((resp (value-of-expression expr sc)))
                                (a-ans (ans-val resp) 'return (extract-sc resp))))
      )
    )
  )

; Global-stmt
(define value-of-global-stmt
  (lambda (stat sc)
    (cases global-stmt stat
      (a-global-stmt (identifier) (a-ans (a-none) '- (add-global-var sc identifier)))
      )
    )
  )

