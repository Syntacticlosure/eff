#lang typed/racket
(require "defs.rkt")
(require syntax/parse/define (for-syntax racket/syntax))

(provide define-effect)
(define-simple-macro (define-effect effname:id
                       (~seq (operation:id optypes ...) (~literal :) returntypes)
                       ...)
  #:with use-effect (format-id #'effname "use-~a" (syntax-e #'effname))
  #:with effect-handler (format-id #'effname "~a-handler" (syntax-e #'effname))
  #:with handle-effect (format-id #'effname "handle-~a" (syntax-e #'effname))
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
    (struct Bind
      ([effect : operation][k : (-> returntypes Freer)]))
    ...
    
    (define-type Freer
      (U Pure Bind ...))
    
    (struct (r) Effect-Handler
      ([val-handler : (-> Any r)]
       [op-handler : (-> operation (-> (-> returntypes r) r))]
       ...))
    
    (define prefab-tag : (Tagof Freer)
      (make-continuation-prompt-tag 'eff-prefab-tag))

    (: use-effect (case-> (-> operation returntypes) ...))
    (define (use-effect effect)
      (cond [(operation? effect)
             (call/shift
              (λ ([k : (-> returntypes Freer)])
                (Bind effect k))
              prefab-tag)]
            ...))
    (: handle-effect (All (r) (-> (-> Any) (Effect-Handler r) r)))
    (define (handle-effect body-thunk handler)
      (match-define (Effect-Handler val-handler op-handler ...) handler)
      (define (run [freer : Freer]) : r
        (match freer
          [(Pure x) (val-handler x)]
          [(Bind effect k) ((op-handler effect) (compose run k))]
          ...))
      (run (call/reset (thunk (Pure (body-thunk)))
                       prefab-tag)))
    (define-simple-macro (effect-handler (~literal :) restype
                                         [val val-body]
                                         [((~literal operation) opargs ...) opk opbody] ...)
      (Effect-Handler
       (λ ([val : Any]) val-body)
       (λ ([effect : operation])
         (match effect
           [(operation opargs ...)
            (λ ([opk : (-> returntypes restype)])
              opbody)]))
       ...))))


