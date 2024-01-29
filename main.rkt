#lang racket

(require (lib "eopl.ss" "eopl"))
(require "parse.rkt"
         "grammar.rkt"
         "envrn.rkt")

(provide (all-defined-out))

(define evaluate
  (lambda (input-string)
    (value-of-program (scan&parse input-string))))
    
  

(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?))
  (none-val
   (n none?))
   
  (func-val
   (f func?)))
   
  

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

; Program
(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (stats)
                 (value-of-stats stats (gb-sc-new))))))
    
  

; Statements
(define value-of-stats
  (lambda (stats sc)
    (cases statements stats
      (a-statement (stat)
                   (value-of-stmt stat sc))
                   
      (cum-statements (stats stat)
                     (let ((answer-stats (value-of-stats stats sc)))
                        (if (not (or (ret-ans? answer-stats) (break-ans? answer-stats) (continue-ans? answer-stats)))
                            (value-of-stmt stat (extract-sc answer-stats))
                            answer-stats))))))
                        
                      
      
    
  

; Statement
(define value-of-stmt
  (lambda (stat sc)
    (cases statement stat
      (a-compound-stmt (cmp-stat) (value-of-compound-stmt cmp-stat sc))
      (a-simple-stmt (sim-stmt) (value-of-simple-stmt sim-stmt sc)))))
      
    
  

; Simple-stmt
(define value-of-simple-stmt
  (lambda (stat sc)
    (cases simple-stmt stat
      (assign-stmt (assign) (value-of-assignment assign sc))
      (glob-stmt (glob) (value-of-global-stmt glob sc))
      (ret-stmt (ret) (value-of-return ret sc))
      (pass-stmt () (a-ans (a-none) '- sc))
      (break-stmt () (a-ans (a-none) 'break sc))
      (continue-stmt () (a-ans (a-none) 'continue sc))
      (simple-print-stmt () (display ""))
      (print-stmt (args) (value-of-print-stmt args)))))
      
    
  

; Assignment-stmt
(define value-of-assignment
  (lambda (stat sc)
    (cases assignment-stmt stat
      (a-assign-stmt (identifier expr) (a-ans (a-none) '- (extend-sc sc identifier (a-thunk expr (copy-of-sc sc))))))))
      
    
  

; Return-stmt
(define value-of-return
  (lambda (stat sc)
    (cases return-stmt stat
      (return-void-stmt () (a-ans (a-none) 'return sc))
      (return-exp-stmt (expr) (let
                                  ((resp (value-of-expression expr sc)))
                                (a-ans (ans-val resp) 'return (extract-sc resp)))))))
      
    
  

; Global-stmt
(define value-of-global-stmt
  (lambda (stat sc)
    (cases global-stmt stat
      (a-global-stmt (identifier) (a-ans (a-none) '- (add-global-var sc identifier))))))
      
    
  

; Compound-stmt
(define value-of-compound-stmt
  (lambda (stat sc)
    (cases compound-stmt stat
      (cmp-function-def (func-def) (value-of-func-def func-def sc))
      (cmp-if-stmt (if-stmt) (value-of-if-stmt if-stmt sc))
      (cmp-for-stmt (for-stmt) (value-of-for-stmt for-stmt sc)))))
      
    
  

; Function_def
(define-datatype func func?
  (a-func
   (identifier symbol?)
   (params (lambda (p) (or (none? p) (params? p))))
   (stats statements?)
   (sc scope?)))

(define value-of-func-def
  (lambda (func-def sc)
    (cases cmp-function-def func-def
      (func-def-with-params
       (identifier params stats)
       (let ((f (a-func identifier params stats (lc-sc-new scope))))
         (a-ans (a-none) '- (extend-sc sc identifier f))))
         
       
      (func-def-no-params 
       (identifier stats)
       (let ((f (a-func identifier (a-none) sts (lc-sc-new scope))))
         (a-ans (a-none) '- (extend-sc sc identifier f)))))))
         
       
      
    
  

; If_stmt
(define value-of-if-stmt
  (lambda (if-stmt sc)
    (cases if-stmt if-s
      (a-if-stmt
       (cond-expr if-stats else-block)
       (let ((resp (value-of-expression cond-expr sc)))
         (if (ans-val resp)
            (value-of-stats stats (extract-sc resp))
            (value-of-else-block else-block (extract-sc resp))))))))
            
         
       
      
    
  

; else block
(define value-of-else-block
  (lambda (eb scope)
    (cases else-block eb
      (a-else-block 
       (else-stats)
       (value-of-stats else-stats sc)))))
      
    
  

; For_stmt
(define-datatype evaluated-list evaluated-list?
  (a-evaluated-list
   (python-list python-list?)
   (sc scope?)))

; for stmt
(define value-of-for-stmt
  (lambda (fs scope)
    (cases for-stmt fs
      (a-for-stmt 
       (iter expr stats)
       (let ((resp (value-of-expression expr sc)))
         (cases evaluated-list (ans-val resp)
           (a-evaluated-list 
            (python-list sc)
            (value-of-for-body iter python-list sc sts (extract-sc resp)))))))))
            
           
         
       
      
    
  

(define value-of-for-body
  (lambda (iter lst stored-sc stats sc)
    (if (null? lst)
       (a-ans (a-none) '- sc)
       (let* ((resp1 (value-of-expression (car lst) stored-sc))
              (resp2 (value-of-stats stats (extend-sc sc iter (ans-val resp1)))))
         (if (break-ans? resp2)
            (a-ans (a-none) '- (extract-sc resp2))
            (value-of-for-body iter (cdr lst) stored-sc stats (extract-sc resp2)))))))
            
         
       
    
  

(define value-of-expression
 (lambda (exp scope)
  (cases expression? exp
   (disjunct-expression (disjunc) (value-of-disjunction disjunc scope)))))



(define value-of-disjunction
 (lambda (disj scope))
 (cases disjunction disj
  (a-disjunction (conjunc) (value-of-conjunction conjunc scope))
  (cum-disjunction (disjunc conjunc) (let ((ans1 (value-of-disjunction disjunc scope)))
                                      (let ((ans2 (value-of-conjunction conjunc (extract-sc ans1))))
                                       (a-ans (or (ans-val ans1) (ans-val ans2)) '- (extract-sc ans2)))))))


(define value-of-conjunction
 (lambda (conjunc scope)
  (cases conjunction conjunc
   (a-conjunction (invers) (value-of-inversion invers scope))
   (cum-conjunction (conjunc invers) (let ((ans1 (value-of-conjunction conjunc scope)))
                                      (let ((ans2 (value-of-inversion invers (extract-sc ans1))))
                                       (a-ans (and (ans-val ans1) (ans-val ans2)) '- (extract-sc ans2))))))))



(define value-of-inversion 
 (lambda (ex scope)
  (cases inversion ex
   (not-of-inversion (invers) (let ((ans (value-of-inversion invers scope)))
                               (a-ans (not (ans-val ans)) '- (extract-sc ans))))
   (a-comparison (comparsion) (value-of-comparison comparsion scope)))))


(define-datatype cmp-answer cmp-answer?
  (a-cmp-answer
   (result boolean?)
   (scope scope?)))   

(define value-of-comparison
 (lambda (ex scope)
  (cases comparison ex
    (sum-comp (sum) (value-of-sum sum scope))
    (eq-comp (eq-sum
                (let ((cmp-ans (value-of-eq-sum eq-sum scope)))
                  (cases cmp-answer cmp-ans
                    (a-cmp-answer (res sc) (a-ans res '- sc))))))
    (lt-comp (lt-sum
              (let ((cmp-ans (value-of-lt-sum lt-sum scope)))
                (cases cmp-answer cmp-ans
                  (a-cmp-answer (res sc) (a-ans res '- sc))))))
    (gt-comp (gt-sum
              (let ((cmp-ans (value-of-gt-sum gt-sum scope)))
                (cases cmp-answer cmp-ans
                  (a-cmp-answer (res sc) (a-ans res '- sc)))))))))


(define value-of-eq-sum
  (lambda (ex scope)
    (cases eq-sum ex
      (a-eq-sum (sum1 sum2)
                (let ((ans1 (value-of-sum sum1 scope))
                      (ans2 (value-of-sum sum2 scope)))
                  (a-cmp-answer (= (ans-val ans1) (ans-val ans2)) (extract-sc ans2)))))))


(define value-of-lt-sum
  (lambda (ex scope)
    (cases lt-sum ex
      (a-lt-sum (sum1 sum2)
                (let ((ans1 (value-of-sum sum1 scope))
                      (ans2 (value-of-sum sum2 scope)))
                  (a-cmp-answer (< (ans-val ans1) (ans-val ans2)) (extract-sc ans2)))))))


