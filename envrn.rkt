#lang debug racket

(require eopl)
(provide (all-defined-out))

(define envrn? 
  (list-of pair?))
  

(define apply-env
  (lambda (env var)
    (let ((found (assoc var env)))
      (if found
          (cdr found)
          (eopl:error 'apply-env "bounded value not found for ~s" var)))))


(define empty-env 
  (lambda () (list)))
  

(define globe 
  (empty-env))
  

(define extend-env
  (lambda (env var val)
    (cons (cons var val) env)))
    
  

(define-datatype scope scope?
  (gb-sc)
  (lc-sc
   (ls-gb-vars (list-of symbol?))
   (env envrn?)))
   
  

(define gb-sc-new 
  (lambda () (gb-sc)))
  

(define lc-sc-new 
  (lambda (sc) (cases scope sc
                 (gb-sc () (lc-sc (list) (empty-env)))
                 (lc-sc (gvl env) (lc-sc (list) env)))))
                 
    
  

(define apply-sc
  (lambda (sc var)
    (cases scope sc
      (gb-sc () (apply-env globe var))
      (lc-sc (ls-gb-vars env)
             (cond
               ((member var ls-gb-vars) (apply-env globe var))
               ((assoc var env) (apply-env env var))
               (else (eopl:error 'apply-sc "bounded value not found for ~s" var)))))))
               
             
      
    
  

(define extend-sc
  (lambda (sc var val)
    (cases scope sc
      (gb-sc () (begin
                  (set! globe (extend-env globe var val))
                  sc))
      (lc-sc (ls-gb-vars env)
             (cond
               ((member var ls-gb-vars) (begin
                                          (set! globe (extend-env globe var val))
                                          sc))
               (else (lc-sc ls-gb-vars (extend-env env var val))))))))
               
             
      
    
  

(define add-env-gb-vars
  (lambda (ls-gb-vars env)
    (if (null? ls-gb-vars)
        env
        (add-env-gb-vars (cdr ls-gb-vars) (extend-env env (car ls-gb-vars) (apply-env globe (car ls-gb-vars)))))))
        
    
  

(define add-to-ls-gb-vars
  (lambda (sc var)
    (cases scope sc
      (gb-sc () sc)
      (lc-sc (ls-gb-vars env)
             (lc-sc (cons var ls-gb-vars) env)))))
             
      
    
  

(define cp-of-sc
  (lambda (sc)
    (cases scope sc
      (gb-sc () (lc-sc '() globe))
      (lc-sc (ls-gb-vars env)
             (lc-sc '() (append (add-env-gb-vars ls-gb-vars env) env))))))
             
      
    
  
