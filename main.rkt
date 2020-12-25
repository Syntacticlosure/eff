#lang typed/racket
(require "defs.rkt" (for-syntax "macro-helpers.rkt"))
(require syntax/parse/define (for-syntax racket/syntax))

(provide define-effect Tagof)
(define-simple-macro (define-effect (~or (effname:id _effpolyvars:id ...)
                                         effname:id)
                       (~seq (operation:id _optypes ...) (~literal :) _returntypes)
                       ...)
  #:do [(define formatter (id-formatter #'effname #'effname))
        (define (generate-operation-temporaries)
          (generate-temporaries #'(operation ...)))]
  #:with (_i_effpolyvars ...) #'(~? (_effpolyvars ...) ())
  #:with (effpolyvars  ...) (generate-temporaries #'(_i_effpolyvars ...))
  #:with (returntypes ...) (replace-ids #'([_i_effpolyvars effpolyvars] ...)
                                     #'(_returntypes ...))
  #:with ((optypes ...) ...) (replace-ids #'([_i_effpolyvars effpolyvars] ...)
                                     #'((_optypes ...) ...))
  #:with use-effect (formatter "use-~a")
  #:with effect-handler (formatter "~a-handler")
  #:with handle-effect (formatter "handle-~a")
  #:with Freer (formatter "~a-Freer")
  #:with (operation? ...) (map (λ (x) (format-id #'effname "~a?" (syntax-e x)))
                               (syntax->list #'(operation ...)))
  #:with ((opargs ...) ...) (map generate-temporaries
                                 (syntax->list #'((optypes ...) ...)))
  #:with (opbody ...) (generate-operation-temporaries)
  #:with (opk ...) (generate-operation-temporaries)
  #:with (Bind ...) (generate-operation-temporaries)
  #:with (op-handler ...) (generate-operation-temporaries)
  (begin
    (struct (effpolyvars ...) operation ([opargs : optypes] ...)) ...
    (define-type (~typeapp effname effpolyvars ...)
      (U (~typeapp operation effpolyvars ...) ...))
    (struct (effpolyvars ...) Bind
      ([effect : (~typeapp operation effpolyvars ...)]
       [k : (-> returntypes (~typeapp Freer effpolyvars ...))]))
    ...
    
    (define-type (~typeapp Freer effpolyvars ...)
      (U Pure (~typeapp Bind effpolyvars ...) ...))
    
    (struct (r v effpolyvars ...) Effect-Handler
      ([val-handler : (-> v r)]
       [op-handler :  (-> (~typeapp operation effpolyvars ...)
                          (-> (-> returntypes r) r))]
       ...))

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
                          (-> (-> (Tagof (~typeapp Freer effpolyvars ...)) v)
                              (Effect-Handler r v effpolyvars ...) r)))
    (define (handle-effect body-thunk handler)
      (match-define (Effect-Handler val-handler op-handler ...) handler)
      (define tag : (Tagof (~typeapp Freer effpolyvars ...))
        (make-continuation-prompt-tag))
      (define result : (Option (Some v)) #f)
      (define (run [freer : (~typeapp Freer effpolyvars ...)]) : r
        (match freer
          [(Pure) (match result
                  [(Some x) (val-handler x)])]
          [(Bind effect k) ((op-handler effect) (compose run k))]
          ...))
      (run (call/reset (thunk (set! result (Some (body-thunk tag)))
                              (Pure))
                       tag)))
    (define-simple-macro (effect-handler (~optional (_initpolytypes (... ...)))
                                         (~literal :) restype
                                         [val (~literal :) vtype val-body]
                                         [((~literal operation) opargs ...) opk opbody] ...)
      #:with (__effpolyvars (... ...)) #'(effpolyvars ...)
      #:with (initpolytypes (... ...)) #'((... ~?) (_initpolytypes (... ...))
                                                        ())
      (let ()
        (define-type __effpolyvars initpolytypes) (... ...)
      (Effect-Handler
       (λ ([val : vtype]) val-body)
       (λ ([effect : (~typeapp operation effpolyvars ...)])
         (match effect
           [(operation opargs ...)
            (λ ([opk : (-> returntypes restype)])
              opbody)]))
       ...)))))
