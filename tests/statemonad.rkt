#lang racket
(require "../main.rkt" rackunit)
(define-control-effect get)
(define-control-effect put)

(define (with-state body)
  (with-effect/control
      ([v (λ (s) v)]
       [(get) k (λ (s) ((k s) s))]
       [(put v) k (λ (s) ((k (void)) v))])
    (body)))

(module+ test
  (check-equal? ((with-state (λ ()
                               (define x (get))
                               (put (+ x 1))
                               (put (+ x 2))
                               (get))) 2) 4))