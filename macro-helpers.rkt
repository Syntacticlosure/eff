#lang racket
(require (for-template racket/base))
(provide (all-defined-out))
(define (compile-time stx)
  #`(define-syntaxes ()
      (begin
        #,stx
        (values))))