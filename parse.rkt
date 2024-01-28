#lang racket

(require "passes/parser.rkt")
(require "passes/lexer.rkt")

(define (lex&parse prog-string)
  (python-parser (lex-this prog-string))
  )

(define (scan&parse file-name)
  (lex&parse (string-join (file->lines file-name)))
  )

(provide (all-defined-out))