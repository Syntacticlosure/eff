#lang racket
(require racket/control syntax/parse/define (for-syntax syntax/id-table)
         (for-syntax "macro-helpers.rkt"))

(provide define-control-effect with-effect/control)
(define-for-syntax control-effect-table (make-free-id-table))

(define-syntax-parser define-control-effect
  [(_ effname:id)
   #:with (markkey) (generate-temporaries '(markkey))
   #:with set-env (compile-time #'(free-id-table-set! control-effect-table
                                                      #'effname
                                                      #'markkey))
   #:with use-markkey
   #'((continuation-mark-set-first #f
                                   markkey
                                   (λ ()
                                     (error (format
                                             "do not have the effect ~a"
                                             'effname)))))
   #`(begin
       set-env
       (define markkey (make-continuation-mark-key))
       (define (effname . args)
         (let ([cached use-markkey])
           (shift-at (car cached) k
                     (apply ((cdr cached) k) args)))))])


(define-syntax (with-effect/control stx)
  (define (iter stx)
    (syntax-parse stx
      [(_ ([val valbody]
           [(effname:id args:id ...) k:id effbody] rest ...) bodies ...)
       #:with markkey (free-id-table-ref control-effect-table #'effname)
       #`(with-continuation-mark markkey (thunk
                                          (cons tag
                                                (λ (k)
                                                  (λ (args ...)
                                                    effbody))))
           #,(iter #'(with-effect/control ([val valbody] rest ...)
                       bodies ...)))]
      [(_ ([val valbody]) bodies ...)
       #'(reset-at tag 
                   (let ([val (let () bodies ...)])
                     valbody))]))
  #`(let ([tag (make-continuation-prompt-tag)])
      #,(iter stx)))

