#lang racket
(require syntax/parse/define
         (for-syntax syntax/id-table)
         "macro-helpers.rkt"
         "scope.rkt")

(provide define-value-effect with-effect/value)
(define-syntax-parser define-value-effect
  [(_ effname:id (~optional default))
   #:with (markkey scope-markkey) (generate-temporaries '(markkey scope-markkey))
   #:with set-env (compile-time #'(free-id-table-set! 
                                   value-effect-table #'effname
                                   #'(markkey scope-markkey)))
   #:with use-eff
   #'(scoped-get (get-root-contmarks) markkey scope-markkey 'effname)
   #`(begin
       set-env
       (define markkey
         (make-continuation-mark-key))
       (define scope-markkey
         (make-continuation-mark-key))
       (define-syntax (effname stx)
         (syntax-case stx ()
           [val (identifier? #'val) #'use-eff]
           [(_ rest (... ...)) #'(use-eff rest (... ...))])))])

(define-syntax-parser with-effect/value
  [(_ ([effid:id val] rest ...) bodies ...)
   #:with (markkey _) (free-id-table-ref value-effect-table
                                         #'effid)
   (scoped-set #'(get-root-contmarks) #'markkey #'val 
               #`(with-effect/value (rest ...) bodies ...))]
  [(_ () body)
   #`body]
  [(_ () bodies ...)
   #`(let () bodies ...)])