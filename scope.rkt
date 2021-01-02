#lang racket
;; (mask effects bodies ...)
(require (only-in '#%unsafe [unsafe-root-continuation-prompt-tag root-tag]))
(provide scoped-get get-root-contmarks
         (for-syntax set-outer-scope scoped-set))

(define (get-root-contmarks)
  (current-continuation-marks (root-tag)))

(define (scoped-get contmarks markkey scope-markkey effname)
  (let ([sc (continuation-mark-set-first contmarks scope-markkey 0)]
        [ms (continuation-mark-set-first contmarks markkey '())])
    (if (> (length ms) sc)
        (list-ref ms sc)
        (error (format "do not have the effect ~a" effname)))))

(define-for-syntax (scoped-set contmarks markkey value body)
  #`(let ([ms (continuation-mark-set-first
               #,contmarks #,markkey '())])
      (with-continuation-mark #,markkey (cons #,value ms)
        #,body)))
               

(define-for-syntax (set-outer-scope contmarks scope-markkey body)
  #`(let ([sc (continuation-mark-set-first
               #,contmarks #,scope-markkey 0)])
      (with-continuation-mark #,scope-markkey (add1 sc)
        #,body)))
