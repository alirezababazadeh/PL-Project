#lang racket

(require "lexer.rkt" 
 "../grammar.rkt")
(require parser-tools/lex
         parser-tools/yacc)
                        

(define python-parser
  (parser
   (start program)
   (end EOF)
   (error void)
   (tokens
    LITERALS KWS OPS LOOP_KWS BOOL_KWS BOOL_OPS
    COND_KWS COND_OPS ARITH_OPS INDEX_OPS END)
    
   (grammar
    (program ((statements) (a-program $1)))

    (statements ((statement SEMICOLON) (a-statement $1))
                ((statements statement SEMICOLON) (cum-statements $1 $2)))
                
    (statement ((compound-stmt) (a-compound-stmt $1))
               ((simple-stmt) (a-simple-stmt $1)))
               
    (simple-stmt ((assignment-stmt) (a-assign-stmt $1))
                 ((global-stmt) (glob-stmt $1))
                 ((return-stmt) (ret-stmt $1))
                 ((PASS) (pass-stmt))
                 ((BREAK) (break-stmt))
                 ((CONTINUE) (continue-stmt))
                 ((PRINT PAR) (simple-print-stmt))
                 ((PRINT LPAR arguments RPAR) (a-print-stmt $3)))
                 
    (compound-stmt ((function-def) (cmp-function-def $1))
                   ((if-stmt) (cmp-if-stmt $1))
                   ((for-stmt) (cmp-for-stmt $1)))
                   
    (assignment-stmt ((ID ASSIGN expression) (a-assign $1 $3)))

    (return-stmt ((RETURN) (return-void-stmt))
                 ((RETURN expression) (return-exp-stmt $2)))
                 
    (global-stmt ((GLOBAL ID) (a-global-stmt $2)))

    (function-def ((DEF ID LPAR params RPAR COLON statements) (func-def-with-params $2 $4 $7))
                  ((DEF ID PAR COLON statements) (func-def-no-params $2 $5)))
                  
    (params ((param) (empty-param $1))
            ((params COMMA param) (func-params $3 $1)))
            
    (param ((ID ASSIGN expression) (with_default $1 $3)))

    (if-stmt ((IF expression COLON statements else-block) (a-if-stmt $2 $4 $5)))

    (else-block ((ELSE COLON statements) (a-else-block $3)))

    (for-stmt ((FOR ID IN expression COLON statements) (a-for-stmt $2 $4 $6)))

    (expression ((disjunction) (disjunct-expression $1)))

    (disjunction ((conjunction) (a-disjunction $1))
                 ((disjunction OR conjunction) (cum-disjunction $1 $3)))
                 
    (conjunction ((inversion) (a-conjunction $1))
                 ((conjunction AND inversion) (cum-conjunction $1 $3)))
                 
    (inversion ((NOT inversion) (not-of-inversion $2))
               ((comparison) (a-comparison $1)))
               
    (comparison ((eq-sum) (eq-comp $1))
                ((lt-sum) (lt-comp $1))
                ((gt-sum) (gt-comp $1))
                ((sum) (sum-comp $1)))
                
    (eq-sum ((sum ISEQ sum) (a-eq-sum $1 $3)))

    (lt-sum ((sum LT sum) (a-lt-sum $1 $3)))

    (gt-sum ((sum BT sum) (a-gt-sum $1 $3)))

    (sum ((sum PLUS term) (plus-sum $1 $3))
         ((sum MINUS term) (minus-sum $1 $3))
         ((term) (term-sum $1)))
         
    (term ((term MULTI factor) (mult-term $1 $3))
          ((term DIV factor) (div-term $1 $3))
          ((factor) (factor-term $1)))
          
    (factor ((PLUS power) (pos-factor $2))
            ((MINUS power) (neg-factor $2))
            ((power) (power-factor $1)))
            
    (power ((atom POW factor) (atom-powered $1 $3))
           ((primary) (a-primary $1)))
           
    (primary ((atom) (a-atom $1))
             ((primary LBRACK expression RBRACK) (arr-access $1 $3))
             ((primary PAR) (func-call-no-arg $1))
             ((primary LPAR arguments RPAR) (func-call-with-args $1 $3)))
             
    (arguments ((expression) (arg-expression $1))
               ((arguments COMMA expression) (args-expression $3 $1)))
               
    (atom ((ID) (a-id $1))
          ((TRUE) (a-bool true))
          ((FALSE) (a-bool false))
          ((NONE) (a-none))
          ((NUMBER) (a-num $1))
          ((python-list) (a-list $1)))
          
    (python-list ((LBRACK expressions RBRACK) (filled-list $2))
          ((BRACK) (empty-list)))
          
    (expressions ((expressions COMMA expression) (cum-expression $3 $1))
                 ((expression) (a-expression $1))))))
                 
(provide (all-defined-out))