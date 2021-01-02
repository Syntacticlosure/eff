#lang racket
(require (for-template racket/base)
         (for-syntax syntax/id-table))
(provide (for-syntax (all-defined-out)))
(define-for-syntax value-effect-table (make-free-id-table))
(define-for-syntax control-effect-table (make-free-id-table))
(define-for-syntax (compile-time stx)
  #`(define-syntaxes ()
      (begin
        #,stx
        (values))))