#lang racket
(require syntax/parse/experimental/template
         syntax/parse
         racket/syntax
         syntax/id-table
         (for-syntax syntax/parse))

(provide ~typeapp id-formatter replace-ids hash-syntax)

(define-template-metafunction (~typeapp stx)
  (syntax-parse stx
    [(_ f args ...) (if (null? (syntax->list #'(args ...)))
                        #'f
                        #'(f args ...))]))

(define (replace-ids tb stx)
  (define mapping-table (make-free-id-table))
  (define (traverse stx)
    (syntax-parse stx
      [(a . b) #`(#,(traverse #'a) .
                                   #,(traverse #'b))]
      [() #'()]
      [x (if (and (identifier? #'x)
                  (free-id-table-ref mapping-table #'x #f))
             (free-id-table-ref mapping-table #'x)
             #'x)]))
  (for-each (lambda (p)
              (match-define (list f t) (syntax->list p))
              (free-id-table-set! mapping-table f t))
            (syntax->list tb))
  (traverse stx))

(define ((id-formatter lctx ident) fs)
  (format-id lctx fs ident))

(define (hash-syntax . args)
  (define (iter args)
    (match args
      [(list key value rest ...) #`('#,key #'#,value
                                           #,@(iter rest))]
      [_ #`()]))
  #`(hash #,@(iter args)))


