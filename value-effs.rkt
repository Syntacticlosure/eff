#lang racket
(require syntax/parse/define
         (for-syntax syntax/id-table)
         (for-syntax "macro-helpers.rkt"))

(provide define-value-effect with-effect/value)
(define-for-syntax value-effect-table (make-free-id-table))
(define-syntax-parser define-value-effect
  [(_ effname:id (~optional default))
   #:with (markkey) (generate-temporaries '(markkey))
   #:with set-env (compile-time #'(free-id-table-set! 
                                   value-effect-table #'effname
                                   #'markkey))
   #:with use-eff
   #'((continuation-mark-set-first #f
                                   markkey
                                   (~? (λ () default)
                                       (λ ()
                                         (error (format
                                                 "do not have the effect ~a"
                                                 'effname))))))
   #`(begin
       set-env
       (define markkey
         (make-continuation-mark-key))
       (define-syntax (effname stx)
         (syntax-case stx ()
           [val (identifier? #'val) #'use-eff]
           [(_ rest (... ...)) #'(use-eff rest (... ...))])))])

(define-syntax-parser with-effect/value
  [(_ ([effid:id val] rest ...) bodies ...)
   #:with markkey (free-id-table-ref value-effect-table
                                     #'effid)
   #`(with-continuation-mark markkey (thunk val)
       (with-effect/value (rest ...) bodies ...))]
  [(_ () body)
   #`body]
  [(_ () bodies ...)
   #`(let () bodies ...)])