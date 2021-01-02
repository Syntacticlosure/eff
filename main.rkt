#lang racket
(require "control-effs.rkt" "value-effs.rkt" "scope.rkt"
         "macro-helpers.rkt"
         syntax/parse/define
         (for-syntax syntax/id-table))
(provide (all-from-out "control-effs.rkt")
         (all-from-out "value-effs.rkt")
         mask-effect)

(define-syntax (mask-effect stx)
  (define (iter stx)
    (syntax-parse stx
      [(_ (effid:id rest ...) bodies ...)
       #:with (_ scope-markkey) (or (free-id-table-ref value-effect-table #'effid #f)
                                    (free-id-table-ref control-effect-table #'effid #f))
       (set-outer-scope #'contmarks #'scope-markkey
                        (iter #`(mask-effect (rest ...) bodies ...)))]
      [(_ () bodies ...)
       #`(let () bodies ...)]))
  #`(let ([contmarks (get-root-contmarks)])
      #,(iter stx)))

   
                                


