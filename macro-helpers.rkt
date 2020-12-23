#lang racket
(require syntax/parse/experimental/template
         syntax/parse
         racket/syntax)

(provide ~typeapp id-formatter)

(define-template-metafunction (~typeapp stx)
  (syntax-parse stx
    [(_ f args ...) (if (null? (syntax->list #'(args ...)))
                        #'f
                        #'(f args ...))]))

(define ((id-formatter lctx ident) fs)
  (format-id lctx fs ident))

