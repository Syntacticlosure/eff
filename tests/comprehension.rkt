#lang racket
(require "../main.rkt" rackunit)
(define-control-effect inlist)
(define-control-effect invector)
(define (a)
  (define n1 (inlist '(1 2 3)))
  (define n2 (invector (vector 4 5 6)))
  (+ n1 n2))


(define (comp/list body-thunk)
  (with-effect/control
    ([v (list v)]
     [(inlist l) k (apply append (map k l))]
     [(invector v) k (apply append (map k (vector->list v)))])
    (body-thunk)))

(define (comp/max body-thunk)
  (with-effect/control
    ([v v]
     [(inlist l) k (apply max (map k l))]
     [(invector v) k (apply max (map k (vector->list v)))])
    (body-thunk)))

                    
(module+ test
  (check-equal? (comp/list a) '(5 6 7 6 7 8 7 8 9))
  (check-equal? (comp/max a) 9))