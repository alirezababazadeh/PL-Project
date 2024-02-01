#lang debug racket

(require (lib "eopl.ss" "eopl"))
(require "parse.rkt"
         "base.rkt"
         "grammar.rkt"
         "envrn.rkt")

(provide (all-defined-out))

(define evaluate
  (lambda (input-string)
    (begin
      (value-of-program (scan&parse input-string))
      (display ""))))
    
  
; Program
(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (stats)
                 (value-of-stats stats (gb-sc-new))))))
    
  

; Statements
(define value-of-stats
  (lambda (ex sc)
    (cases statements ex
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
      (a-assign-stmt (assign) (value-of-assignment assign sc))
      (glob-stmt (glob) (value-of-global-stmt glob sc))
      (ret-stmt (ret) (value-of-return ret sc))
      (pass-stmt () (a-ans (none-stmt) '- sc))
      (break-stmt () (a-ans (none-stmt) 'break sc))
      (continue-stmt () (a-ans (none-stmt) 'continue sc))
      (simple-print-stmt () (displayln ""))
      (a-print-stmt (pstmt) (value-of-print pstmt sc)))))
      
    
  

; Assignment-stmt
(define value-of-assignment
  (lambda (stat sc)
    (cases assignment-stmt stat
      (a-assign (identifier expr) (a-ans (none-stmt) '- (extend-sc sc identifier (a-thunk expr (cp-of-sc sc))))))))
      
    
  

; Return-stmt
(define value-of-return
  (lambda (stat sc)
    (cases return-stmt stat
      (return-void-stmt () (a-ans (none-stmt) 'return sc))
      (return-exp-stmt (expr) (let
                                ((resp (value-of-expression expr sc)))
                                (a-ans (ans-val resp) 'return (extract-sc resp)))))))
      
; Global-stmt
(define value-of-global-stmt
  (lambda (stat sc)
    (cases global-stmt stat
      (a-global-stmt (identifier) (a-ans (none-stmt) '- (add-to-ls-gb-vars sc identifier))))))
      
; Compound-stmt
(define value-of-compound-stmt
  (lambda (stat sc)
    (cases compound-stmt stat
      (cmp-function-def (func-def) (value-of-func-def func-def sc))
      (cmp-if-stmt (if-stmt) (value-of-if-stmt if-stmt sc))
      (cmp-for-stmt (for-stmt) (value-of-for-stmt for-stmt sc)))))
      

; Function_def
(define value-of-func-def
  (lambda (func-def sc)
    (cases function-def func-def
      (func-def-with-params
       (identifier params stats)
       (let ((f (a-func identifier params stats (lc-sc-new sc))))
         (a-ans (none-stmt) '- (extend-sc sc identifier f))))
         
       
      (func-def-no-params 
       (identifier stats)
       (let ((f (a-func identifier (none-stmt) stats (lc-sc-new sc))))
         (a-ans (none-stmt) '- (extend-sc sc identifier f)))))))
         

; If_stmt
(define value-of-if-stmt
  (lambda (ex sc)
    (cases if-stmt ex
      (a-if-stmt
       (cond-expr if-stats else-block)
       (let ((resp (value-of-expression cond-expr sc)))
         (if (ans-val resp)
            (value-of-stats if-stats (extract-sc resp))
            (value-of-else-block else-block (extract-sc resp))))))))
            
         
       
      
    
  

; else block
(define value-of-else-block
  (lambda (ex sc)
    (cases else-block ex
      (a-else-block 
       (else-stats)
       (value-of-stats else-stats sc)))))
      
(define python-list->list-value
 (lambda (py-lst)
  (cases python-list py-lst
   (filled-list (exprs) (expressions->list-val exprs))
   (empty-list () '()))))

; for stmt
(define value-of-for-stmt
  (lambda (fs sc)
    (cases for-stmt fs
      (a-for-stmt 
       (iter expr stats)
       (let ((resp (value-of-expression expr sc)))
         (cases evaluated-list (ans-val resp)
           (a-evaluated-list 
            (python-list sc)
            (value-of-for-body iter (python-list->list-value python-list) sc stats (extract-sc resp)))))))))

(define value-of-for-body
  (lambda (iter lst stored-sc stats sc)
    (if (null? lst)
       (a-ans (none-stmt) '- sc)
       (let* ((resp1 (value-of-expression (car lst) stored-sc))
              (resp2 (value-of-stats stats (extend-sc sc iter (ans-val resp1)))))
         (if (break-ans? resp2)
            (a-ans (none-stmt) '- (extract-sc resp2))
            (value-of-for-body iter (cdr lst) stored-sc stats (extract-sc resp2)))))))
  
       
(define value-of-expression
 (lambda (exp scope)
  (cases expression exp
   (disjunct-expression (disjunc) (value-of-disjunction disjunc scope)))))



(define value-of-disjunction
 (lambda (disj scope)
  (cases disjunction disj
    (a-disjunction (conjunc) (value-of-conjunction conjunc scope))
    (cum-disjunction (disjunc conjunc) (let ((ans1 (value-of-disjunction disjunc scope)))
                                        (let ((ans2 (value-of-conjunction conjunc (extract-sc ans1))))
                                         (a-ans (or (ans-val ans1) (ans-val ans2)) '- (extract-sc ans2))))))))


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
    (eq-comp (eq-sum)
             (let ((cmp-ans (value-of-eq-sum eq-sum scope)))
               (cases cmp-answer cmp-ans
                 (a-cmp-answer (res sc) (a-ans res '- sc)))))
    (lt-comp (lt-sum)
             (let ((cmp-ans (value-of-lt-sum lt-sum scope)))
               (cases cmp-answer cmp-ans
                 (a-cmp-answer (res sc) (a-ans res '- sc)))))
    (gt-comp (gt-sum)
             (let ((cmp-ans (value-of-gt-sum gt-sum scope)))
               (cases cmp-answer cmp-ans
                 (a-cmp-answer (res sc) (a-ans res '- sc)))))
    (sum-comp (sum) (value-of-sum sum scope)))))


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


(define value-of-gt-sum
  (lambda (ex scope)
    (cases gt-sum ex
      (a-gt-sum (sum1 sum2)
                (let ((ans1 (value-of-sum sum1 scope))
                      (ans2 (value-of-sum sum2 scope)))
                  (a-cmp-answer (> (ans-val ans1) (ans-val ans2)) (extract-sc ans2)))))))



(define value-of-sum
  (lambda (ex scope)
    (cases sum ex
      (plus-sum (sum term)
               (let ((ans1 (value-of-sum sum scope)))
                 (let ((ans2 (value-of-term term (extract-sc ans1))))
                   (let ((exp1 (ans-val ans1))
                         (exp2 (ans-val ans2))
                         (scope (extract-sc ans2)))
                     (cond
                       ((evaluated-list? exp1)
                        (cases evaluated-list exp1
                          (a-evaluated-list (py-list1 sc1)
                                        (cases evaluated-list exp2
                                          (a-evaluated-list (py-list2 sc2)
                                                        (a-ans (a-evaluated-list (append py-list1 py-list2) scope) '- scope))))))
                       (else (a-ans (+ exp1 exp2) '- scope)))))))
      (minus-sum (sum term)
               (let ((ans1 (value-of-sum sum scope)))
                 (let ((ans2 (value-of-term term (extract-sc ans1))))
                   (let ((exp1 (ans-val ans1))
                         (exp2 (ans-val ans2))
                         (scope (extract-sc ans2)))
                     (a-ans (- exp1 exp2) '- scope)))))
      (term-sum (term)
                (value-of-term term scope)))))


(define value-of-term
  (lambda (ex scope)
    (cases term ex
      (mult-term (term factor)
               (let ((ans1 (value-of-term term scope)))
                 (let ((exp1 (ans-val ans1))
                       (scope (extract-sc ans1)))
                      (if (zero? exp1)
                        (a-ans 0 '- scope)
                        (let ((ans2 (value-of-factor factor scope)))
                          (a-ans (* exp1 (ans-val ans2)) '- (extract-sc ans2)))))))
      (div-term (term factor)
               (let ((ans1 (value-of-term term scope)))
                 (let ((ans2 (value-of-factor factor (extract-sc ans1))))
                   (a-ans (real->double-flonum (/ (ans-val ans1) (ans-val ans2))) '- (extract-sc ans2)))))
      (factor-term (factor)
                   (value-of-factor factor scope)))))

(define value-of-factor
  (lambda (ex scope)
    (cases factor ex
      (pos-factor (pow)
                  (let ((ans (value-of-power pow scope)))
                    (a-ans (ans-val ans) '- (extract-sc ans))))
      (neg-factor (pow)
                  (let ((ans (value-of-power pow scope)))
                    (a-ans (- (ans-val ans)) '- (extract-sc ans))))
      (power-factor (pow)
                    (value-of-power pow scope)))))


(define value-of-power
  (lambda (ex scope)
    (cases power ex
      (atom-powered (atom factor)
             (let ((ans1 (value-of-atom atom scope)))
               (let ((ans2 (value-of-factor factor (extract-sc ans1))))
                 (a-ans (expt (ans-val ans1) (ans-val ans2)) '- (extract-sc ans2)))))
      (a-primary (primary)
                 (value-of-primary primary scope)))))

(define expressions->list-val
 (lambda (exps)
  (cases expressions exps
   (cum-expression (exprs expr) (append (expressions->list-val exprs) (list expr)))
   (a-expression (expr) (list expr)))))
  
(define list-refrence 
 (lambda (lst-exps idx)
  (cases python-list lst-exps
   (filled-list (exprs) (list-ref (expressions->list-val exprs) idx))
   (empty-list () -1))))

(define value-of-primary
  (lambda (ex scope)
    (cases primary ex
      (a-atom (atom)
              (value-of-atom atom scope))
      (arr-access (primary expr)
                  (let ((ans1 (value-of-primary primary scope)))
                    (let ((ans2 (value-of-expression expr (extract-sc ans1))))
                      (cases evaluated-list (ans-val ans1)
                        (a-evaluated-list (py-list sc)
                                      (a-ans (ans-val (value-of-expression (list-refrence py-list (ans-val ans2)) sc)) '- scope))))))
      (func-call-no-arg (primary)
                        (let ((ans (value-of-primary primary scope)))
                          (a-ans (ans-val (apply-function (ans-val ans) '() (extract-sc ans))) '- (extract-sc ans))))
      (func-call-with-args (primary arguments)
                           (let ((ans (value-of-primary primary scope)))
                             (a-ans (ans-val (apply-function (ans-val ans) arguments (extract-sc ans))) '- (extract-sc ans)))))))


(define value-of-thunk
  (lambda (ex)
    (cases thunk ex
      (a-thunk (exp sc)
              (ans-val (value-of-expression exp sc))))))


(define value-of-param-with-default
  (lambda (ex scope)
    (cases param ex
      (with_default (identifier expr)
                    (let ((exp-val (ans-val (value-of-expression expr scope))))
                      (a-ans exp-val '- (extend-sc scope identifier exp-val)))))))


(define arguments->list-val
 (lambda (args-o scope)
        (cases arguments args-o
         (arg-expression (expr) (list (ans-val (value-of-expression expr scope))))
         (args-expression (args expr) (append (arguments->list-val args scope) (list (ans-val (value-of-expression expr scope))))))))

(define value-of-print
  (lambda (args scope)
    (begin
      (display-lines (map repr (arguments->list-val args scope)))
      (a-ans (none-stmt) '- scope))))

(define params->list-value
 (lambda (pms)
  (if (none? pms)
   '()
    (cases params pms
     (empty-param (param) (list param))
     (func-params (param rest-params) (append (params->list-value rest-params) (list param)))))))

(define get-args-raw-list
 (lambda (ars)
  (if (null? ars)
    '()
    (cases arguments ars
     (arg-expression (expr) (list expr))
     (args-expression (args expr) (append (get-args-raw-list args) (list expr)))))))

(define apply-function
  (lambda (ex arg-list outer-scope)
      (cases func ex
        (a-func (identifier params stats sc)
                (let ((sc (extend-sc sc identifier ex)))
                  (let ((sc (add-params-to-scope (params->list-value params) sc)))
                    (let ((thunk-scope (cp-of-sc outer-scope)))
                      (let ((scope (add-args-to-scope (get-args-raw-list arg-list) (params->list-value params) sc thunk-scope)))
                        (value-of-stats stats scope)))))))))

(define add-params-to-scope
  (lambda (params scope)
      (if (null? params)
          scope
          (let ((ans (value-of-param-with-default (car params) scope)))
            (add-params-to-scope (cdr params) (extract-sc ans))))))


(define add-args-to-scope
  (lambda (arg-list params scope thunk-scope)
    (if (null? arg-list)
        scope
        (cases param (car params)
          (with_default (ID exp)
                        (add-args-to-scope
                           (cdr arg-list)
                           (cdr params)
                           (extend-sc scope ID (a-thunk (car arg-list) thunk-scope))
                           thunk-scope))))))
                                


(define value-of-atom
  (lambda (atom scope)
     (cond
       ((symbol? atom)
        (let ((scope-val (apply-sc scope atom)))
          (if (thunk? scope-val)
              (let ((exp-val (value-of-thunk scope-val)))
                (a-ans exp-val '- (extend-sc scope atom exp-val)))
              (a-ans scope-val '- scope))))
      ((python-list? atom) (a-ans (a-evaluated-list atom (cp-of-sc scope)) '- scope))
      (#t (a-ans atom '- scope)))))


(evaluate "test.py")