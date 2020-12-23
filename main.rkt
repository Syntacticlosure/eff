#lang typed/racket
(require "defs.rkt")
(require syntax/parse/define (for-syntax racket/syntax))

(provide define-effect Tagof)
(define-simple-macro (define-effect effname:id
                       (~seq (operation:id optypes ...) (~literal :) returntypes)
                       ...)
  #:with use-effect (format-id #'effname "use-~a" (syntax-e #'effname))
  #:with effect-handler (format-id #'effname "~a-handler" (syntax-e #'effname))
  #:with handle-effect (format-id #'effname "handle-~a" (syntax-e #'effname))
  #:with Freer (format-id #'effname "~a-Freer" #'effname)
  #:with (operation? ...) (map (λ (x) (format-id #'effname "~a?" (syntax-e x)))
                               (syntax->list #'(operation ...)))
  #:with ((opargs ...) ...) (map generate-temporaries
                                 (syntax->list #'((optypes ...) ...)))
  #:with (opbody ...) (generate-temporaries #'(operation ...))
  #:with (opk ...) (generate-temporaries #'(operation ...))
  #:with (Bind ...) (generate-temporaries #'(operation ...))
  #:with (op-handler ...) (generate-temporaries #'(operation ...))
  (begin
    (struct operation ([opargs : optypes] ...)) ...
    (define-type effname (U operation ...))
    (struct (v) Bind
      ([effect : operation][k : (-> returntypes (Freer v))]))
    ...
    
    (define-type (Freer v)
      (U (Pure v) (Bind v) ...))
    
    (struct (r v) Effect-Handler
      ([val-handler : (-> v r)]
       [op-handler : (-> operation (-> (-> returntypes r) r))]
       ...))

    (: use-effect (All (a) (case-> (-> (Tagof (Freer a)) operation returntypes) ...)))
    (define (use-effect tag effect)
      (cond [(operation? effect)
             (call/shift
              (λ ([k : (-> returntypes (Freer a))])
                (Bind effect k))
              tag)]
            ...))
    (: handle-effect (All (r v) (-> (-> (Tagof (Freer v)) v) (Effect-Handler r v) r)))
    (define (handle-effect body-thunk handler)
      (match-define (Effect-Handler val-handler op-handler ...) handler)
      (define tag : (Tagof (Freer v)) (make-continuation-prompt-tag))
      (define (run [freer : (Freer v)]) : r
        (match freer
          [(Pure x) (val-handler x)]
          [(Bind effect k) ((op-handler effect) (compose run k))]
          ...))
      (run (call/reset (thunk (Pure (body-thunk tag)))
                       tag)))
    (define-simple-macro (effect-handler (~literal :) restype
                                         [val (~literal :) vtype val-body]
                                         [((~literal operation) opargs ...) opk opbody] ...)
      (Effect-Handler
       (λ ([val : vtype]) val-body)
       (λ ([effect : operation])
         (match effect
           [(operation opargs ...)
            (λ ([opk : (-> returntypes restype)])
              opbody)]))
       ...))))


