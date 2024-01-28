#lang racket

(require eopl)
(provide (all-defined-out))

(define envrn? 
  (list-of pair?)
  )

(define apply-env
  (lambda (env var)
    (let ((found (assoc var env)))
      (if found
          (cdr found)
          (eopl:error 'apply-env "bounded value not found for ~s" var)
          )
      )
    )
  )

(define empty-env 
  (lambda () (list))
  )

(define globe 
  (empty-env)
  )

(define extend-env
  (lambda (env var val)
    (cons (cons var val) env)
    )
  )
