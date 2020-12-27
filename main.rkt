#lang typed/racket
(require "defs.rkt" (for-syntax "macro-helpers.rkt"))
(require syntax/parse/define (for-syntax racket/syntax racket/sequence))

(provide define-effect Tagof effect-handler)
(define-simple-macro (define-effect (~or (effname:id _effpolyvars ...)
                                         effname:id)
                       (~seq (operation:id _optypes ...) (~literal :) _returntypes)
                       ...)
  #:do [(define formatter (id-formatter #'effname #'effname))
        (define (generate-operation-temporaries)
          (generate-temporaries #'(operation ...)))]
  #:with (replace-tb effpolyvars  ...) (check-type-vars (attribute _effpolyvars))
  #:with (returntypes ...) (replace-ids #'replace-tb
                                        #'(_returntypes ...))
  #:with ((optypes ...) ...) (replace-ids #'replace-tb
                                          #'((_optypes ...) ...))
  #:with use-effect (formatter "use-~a")
  #:with handle-effect (formatter "handle-~a")
  #:with Freer (formatter "~a-Freer")
  #:with Effect-Tag (formatter "~a-Tag")
  #:with make-effect-tag (formatter "make-~a-tag")
  #:with (operation? ...) (map (λ (x) (format-id #'effname "~a?" (syntax-e x)))
                               (syntax->list #'(operation ...)))
  #:with ((opargs ...) ...) (map generate-temporaries
                                 (syntax->list #'((optypes ...) ...)))
  #:with (Bind ...) (generate-operation-temporaries)
  #:with (op-handler ...) (generate-operation-temporaries)
  #:with macro-env (hash-syntax 'effpolyvars #'(effpolyvars ...)
                                'returntypes #'(returntypes ...)
                                'optypes #'((optypes ...) ...)
                                'use-effect #'use-effect
                                'handle-effect #'handle-effect
                                'Freer #'Freer
                                'make-effect-tag #'make-effect-tag
                                'operation #'(operation ...)
                                'Bind #'(Bind ...))
  (begin
    (define-syntax effname macro-env)
    (struct (effpolyvars ...) operation ([opargs : optypes] ...)) ...
    (struct (effpolyvars ...) Bind
      ([effect : (~typeapp operation effpolyvars ...)]
       [k : (-> returntypes (~typeapp Freer effpolyvars ...))]))
    ...
    (define-type (~typeapp Freer effpolyvars ...)
      (U Pure (~typeapp Bind effpolyvars ...) ...))
    (define-type (~typeapp Effect-Tag effpolyvars ...)
      (Tagof (~typeapp Freer effpolyvars ...)))
    (define-syntax-rule (make-effect-tag effpolyvars ...)
      (ann (make-continuation-prompt-tag)
           (~typeapp Effect-Tag effpolyvars ...)))
    
    (: use-effect (All (effpolyvars ...)
                       (case-> (-> (Tagof (~typeapp Freer effpolyvars ...))
                                   (~typeapp operation effpolyvars ...) returntypes)
                               ...)))
    (define (use-effect tag effect)
      (cond [(operation? effect)
             (call/shift
              (λ ([k : (-> returntypes (~typeapp Freer effpolyvars ...))])
                (Bind effect k))
              tag)]
            ...))
    (: handle-effect (All (r v effpolyvars ...)
                          (-> (Tagof (~typeapp Freer effpolyvars ...)) (-> v)
                              (-> v r) ;; val handler
                              (-> (~typeapp operation effpolyvars ...)
                                  (-> returntypes r) r) ...  ;;ophandlers
                                                        r)))
    (define (handle-effect tag body-thunk val-handler op-handler ...)
      (define result : (Option (Some-Val v)) #f)
      (define (run [freer : (~typeapp Freer effpolyvars ...)]) : r
        (match freer
          [(Pure) (match result
                    [(Some-Val x) (val-handler x)])]
          [(Bind effect k) (op-handler effect (compose run k))]
          ...))
      (run (call/reset (thunk (set! result (Some-Val (body-thunk)))
                              (Pure))
                       tag)))))



(define-simple-macro (effect-handler (~optional (~seq (~or #:∀ #:forall)
                                                             (_handlerpolyvars:id ...)))
                                     (~or (effname:id _initpolytypes ...) effname:id)
                                     (~literal :) restype
                                     [val (~literal :) vtype val-body]
                                     [(_operation:id opargs ...) opk opbody] ...)
  #:do [(define macro-env (syntax-local-value #'effname))
        (unless (hash? macro-env)
          (raise-syntax-error 'effect-existence-check
                              (format "~a is not a defined effect." (syntax-e #'effname))))]
  #:with Freer (hash-ref macro-env 'Freer)
  #:with (operation ...) (hash-ref macro-env 'operation)
  #:with (returntypes ...) (hash-ref macro-env 'returntypes)
  #:with (effpolyvars ...) (hash-ref macro-env 'effpolyvars)
  #:with handle-effect (hash-ref macro-env 'handle-effect)
  #:with (initpolytypes ...) #'(~? (_initpolytypes ...)
                                   ())
  #:with (handlerpolyvars ...) #'(~? (_handlerpolyvars ...)
                                     ())
  #:do [(for-each
         (λ (o _o)
           (unless (free-identifier=? o _o)
             (raise-syntax-error 'operation-check
                                 (format "expect an operation ~a, but got ~a"
                                         (syntax-e o) (syntax-e _o)) _o)))
         (syntax->list #'(operation ...))
         (syntax->list #'(_operation ...)))
        (unless (= (length (syntax->list #'(initpolytypes ...)))
                   (length (syntax->list #'(effpolyvars ...))))
          (raise-syntax-error 'effpolyvars-check
                              "wrong number of types to instantiate an effect"
                              #'(effname _initpolytypes ...)))
        (for ([id (in-syntax #'(effname _operation ...))])
          (syntax-parse-state-cons! 'literals id))]
  
  (λ #:forall (handlerpolyvars ...)
    ([tag : (Tagof (~typeapp Freer initpolytypes ...))]
     [body-thunk : (-> vtype)]) : restype
    (define-type effpolyvars initpolytypes) ...
    (handle-effect tag body-thunk
                   (λ ([val : vtype]) val-body)
                   (λ ([effect : (~typeapp operation initpolytypes ...)]
                       [opk : (-> returntypes restype)])
                     (match effect
                       [(operation opargs ...)
                        opbody]))
                   ...)))

