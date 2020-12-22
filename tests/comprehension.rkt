#lang typed/racket
(require "../main.rkt")
(require typed/rackunit)
(define-effect Comprehension
  (in-list (Listof Any)) : Any
  (in-vector (Vectorof Any)) : Any
  (guard Boolean) : Void)

(define (a)
  (define n1 (use-Comprehension (in-list '(1 2 3))))
  (define n2 (use-Comprehension (in-vector (vector 4 5 6))))
  (+ (cast n1 Integer) (cast n2 Integer)))

(define (possibe-triangles [lines : (Listof Integer)])
  (define l1 (use-Comprehension (in-list lines)))
  (define l2 (use-Comprehension (in-list lines)))
  (define l3 (use-Comprehension (in-list lines)))
  (define _l1 (cast l1 Integer))
  (define _l2 (cast l2 Integer))
  (define _l3 (cast l3 Integer))
  (use-Comprehension (guard (and (> (+ _l1 _l2) _l3)
                                 (> (+ _l1 _l3) _l2)
                                 (> (+ _l2 _l3) _l1))))
  (list _l1 _l2 _l3))
      

(define comp/list
  (Comprehension-handler : (Listof Any)
                         [v (list v)]
                         [(in-list l) k (apply append (map k l))]
                         [(in-vector v) k (apply append (map k (vector->list v)))]
                         [(guard b) k (if b (k (void)) '())]))

(define comp/max
  (Comprehension-handler : Integer
                         [v (cast v Integer)]
                         [(in-list l) k
                                      (apply max (map k (cast l
                                                              (Listof Integer))))]
                         [(in-vector v) k
                                        (apply max (map k (cast (vector->list v)
                                                                (Listof Integer))))]
                         [(guard b) k (if b (k (void)) -999999)]))

                    
(module+ test
  (check-equal? (handle-Comprehension a comp/list) '(5 6 7 6 7 8 7 8 9))
  (check-equal? (handle-Comprehension a comp/max) 9))