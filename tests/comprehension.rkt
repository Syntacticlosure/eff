#lang typed/racket
(require "../main.rkt")
(require typed/rackunit)
(define-effect (Comprehension a)
  (in-list (Listof a)) : a
  (in-vector (Vectorof a)) : a
  (guard Boolean) : Void)
(define tag (make-Comprehension-tag Integer))
(define (a)
  (define n1 (use-Comprehension tag (in-list '(1 2 3))))
  (define n2 (use-Comprehension tag (in-vector (vector 4 5 6))))
  (+ n1 n2))


(define comp/list
  (effect-handler
   #:forall (a b) (Comprehension a) : (Listof b)
   [v : b (list v)]
   [(in-list l) k (apply append (map k l))]
   [(in-vector v) k (apply append (map k (vector->list v)))]
   [(guard b) k (if b (k (void)) '())]))

(define comp/max
  (effect-handler
   (Comprehension Integer) : Integer
   [v : Integer v]
   [(in-list l) k (apply max (map k l))]
   [(in-vector v) k (apply max (map k (vector->list v)))]
   [(guard b) k (if b (k (void)) -999999)]))

                    
(module+ test
  (check-equal? (comp/list tag a) '(5 6 7 6 7 8 7 8 9))
  (check-equal? (comp/max tag a) 9))