#lang typed/racket
(require "../main.rkt")
(require typed/rackunit)
(define-effect State
  (get) : Integer
  (set-state Integer) : Void)

(define #:∀ (a) (add1 [tag : (Tagof (State-Freer a))])
  (define a (use-State tag (get)))
  (use-State tag (set-state (+ a 1)))
  a)

(define #:∀ (a) (2times [tag : (Tagof (State-Freer a))])
  (define a (use-State tag (get)))
  (use-State tag (set-state (* a 2))))

(define ret-and-state
  (State-handler : (-> Integer (Pairof Integer Integer))
   [val : Integer (lambda ([s : Integer]) (cons val s))]
   [(get) k (lambda ([s : Integer]) ((k s) s))]
   [(set-state _s) k (lambda ([s : Integer]) ((k (void)) _s))]))

(define a (handle-State (λ ([tag : (Tagof (State-Freer Integer))]) (add1 tag) (add1 tag)) ret-and-state))
(define b (handle-State (λ ([tag : (Tagof (State-Freer Integer))]) (2times tag) (add1 tag)) ret-and-state))

(module+ test
  (check-equal? (a 3) '(4 . 5))
  (check-equal? (b 3) '(6 . 7)))
