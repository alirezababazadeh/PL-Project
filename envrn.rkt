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

(define-datatype scope scope?
  (gb-sc)
  (lc-sc
   (ls-gb-vars (list-of symbol?))
   (env envrn?)
   )
  )

(define gb-sc-new 
  (lambda () (gb-sc))
  )

(define lc-sc-new 
  (lambda (sc) (cases scope sc
                 (gb-sc () (lc-sc (list) (empty-env)))
                 (lc-sc (gvl env) (lc-sc (list) env))
                 )
    )
  )
