#lang typed/racket
(require "../main.rkt")
(require typed/rackunit)
(define-effect State
  (get) : Integer
  (set-state Integer) : Void)
(define tag (make-State-tag))
(define (add1)
  (define a (use-State tag (get)))
  (use-State tag (set-state (+ a 1)))
  a)

(define (2times)
  (define a (use-State tag (get)))
  (use-State tag (set-state (* a 2))))

(define ret-and-state
  (effect-handler
   State : (-> Integer (Pairof Integer Integer))
   [val : Integer (lambda ([s : Integer]) (cons val s))]
   [(get) k (lambda ([s : Integer]) ((k s) s))]
   [(set-state _s) k (lambda ([s : Integer]) ((k (void)) _s))]))

(define a (ret-and-state tag (thunk (add1) (add1))))
(define b (ret-and-state tag (thunk (2times) (add1))))

(module+ test
  (check-equal? (a 3) '(4 . 5))
  (check-equal? (b 3) '(6 . 7)))
