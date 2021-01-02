#lang racket
(require racket/control syntax/parse/define (for-syntax syntax/id-table)
         "macro-helpers.rkt"
         "scope.rkt")

(provide define-control-effect with-effect/control)

(define-syntax-parser define-control-effect
  [(_ effname:id)
   #:with (markkey scope-markkey) (generate-temporaries '(markkey scope-markkey))
   #:with set-env (compile-time #'(free-id-table-set! control-effect-table
                                                      #'effname
                                                      #'(markkey scope-markkey)))
   #:with use-markkey
   #'(scoped-get (get-root-contmarks) markkey scope-markkey 'effname)
   #`(begin
       set-env
       (define markkey (make-continuation-mark-key))
       (define scope-markkey (make-continuation-mark-key))
       (define (effname . args)
         (let ([cached use-markkey])
           (shift-at (car cached) k
                     (apply ((cdr cached) k) args)))))])


(define-syntax (with-effect/control stx)
  (define (iter stx)
    (syntax-parse stx
      [(_ ([val valbody]
           [(effname:id args:id ...) k:id effbody] rest ...) bodies ...)
       #:with (markkey _) (free-id-table-ref control-effect-table #'effname)
       (scoped-set #'(get-root-contmarks) #'markkey
                   #`(cons tag
                           (λ (k)
                             (λ (args ...)
                               effbody)))
                   (iter #'(with-effect/control ([val valbody] rest ...)
                             bodies ...)))]
      [(_ ([val valbody]) bodies ...)
       #'(reset-at tag 
                   (let ([val (let () bodies ...)])
                     valbody))]))
  #`(let ([tag (make-continuation-prompt-tag)])
      #,(iter stx)))

