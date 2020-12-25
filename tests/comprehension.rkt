#lang typed/racket
(require "../main.rkt")
(require typed/rackunit)
(define-effect Comprehension
  (in-list (Listof Integer)) : Integer
  (in-vector (Vectorof Integer)) : Integer
  (guard Boolean) : Void)

(define (a [tag : (Tagof Comprehension-Freer)])
  (define n1 (use-Comprehension tag (in-list '(1 2 3))))
  (define n2 (use-Comprehension tag (in-vector (vector 4 5 6))))
  (+ n1 n2))


(define comp/list
  (Comprehension-handler : (Listof Integer)
                         [v : Integer (list v)]
                         [(in-list l) k (apply append (map k l))]
                         [(in-vector v) k (apply append (map k (vector->list v)))]
                         [(guard b) k (if b (k (void)) '())]))

(define comp/max
  (Comprehension-handler : Integer
                         [v : Integer v]
                         [(in-list l) k
                                      (apply max (map k l))]
                         [(in-vector v) k
                                        (apply max (map k (vector->list v)))]
                         [(guard b) k (if b (k (void)) -999999)]))

                    
(module+ test
  (check-equal? (handle-Comprehension a comp/list) '(5 6 7 6 7 8 7 8 9))
  (check-equal? (handle-Comprehension a comp/max) 9))