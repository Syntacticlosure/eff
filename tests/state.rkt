#lang typed/racket
(require "../main.rkt")
(require typed/rackunit)
(define-effect State
  (get) : Integer
  (set-state Integer) : Void)

(define (add1)
  (define a (use-State (get)))
  (use-State (set-state (+ a 1)))
  a)

(define (2times)
  (define a (use-State (get)))
  (use-State (set-state (* a 2))))

(define ret-and-state
  (State-handler : (-> Integer (Pairof Any Integer))
   [val (lambda ([s : Integer]) (cons val s))]
   [(get) k (lambda ([s : Integer]) ((k s) s))]
   [(set-state _s) k (lambda ([s : Integer]) ((k (void)) _s))]))

(define a (handle-State (thunk (add1) (add1)) ret-and-state))
(define b (handle-State (thunk (2times) (add1)) ret-and-state))

(module+ test
  (check-equal? (a 3) '(4 . 5))
  (check-equal? (b 3) '(6 . 7)))