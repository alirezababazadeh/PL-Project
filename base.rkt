#lang racket

(require (lib "eopl.ss" "eopl"))
(require "grammar.rkt"
         "envrn.rkt")

(define expval?
  (lambda (e) (or (number? e) (boolean? e) (none? e) (func? e) (evaluated-list? e))))
   
  

(define-datatype answer answer?
  (a-ans
   (value expval?)
   (msg symbol?)
   (sc scope?)))

(define ans-val
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

(define-datatype func func?
  (a-func
   (identifier symbol?)
   (params (lambda (p) (or (none? p) (params? p))))
   (stats statements?)
   (sc scope?)))

(define-datatype evaluated-list evaluated-list?
  (a-evaluated-list
   (python-list python-list?)
   (sc scope?)))

(provide (all-defined-out))