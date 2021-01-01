#lang racket
(require "../main.rkt" rackunit)
(define-value-effect use-state)
;;react hooks style stateful functions
(define (handle-state f)
  (define index 0)
  (define states '())
  (λ args
    (with-effect/value
        ([use-state (λ (init)
                      (if (>= index (length states))
                          (let ([b (box init)])
                            (set! states (cons b states))
                            (set! index (add1 index))
                            (cons init (λ (v) (set-box! b v))))
                          (let ([b (list-ref states (- (length states) 1 index))])
                            (set! index (add1 index))
                            (cons (unbox b) (λ (v) (set-box! b v))))))])
      (set! index 0)
      (apply f args))))

(define counter
  (handle-state (thunk
                 (match-define (cons x set-x) (use-state 0))
                 (set-x (add1 x))
                 x)))

(define counter1
  (handle-state (thunk
                 (match-define (cons x set-x) (use-state 0))
                 (match-define (cons y set-y) (use-state 1))
                 (set-x (add1 x))
                 (set-y (* y 2))
                 (cons x y))))

(module+ test
  (check-equal? (counter) 0)
  (check-equal? (counter) 1)
  (check-equal? (counter) 2)

  (check-equal? (counter1) (cons 0 1))
  (check-equal? (counter1) (cons 1 2))
  (check-equal? (counter1) (cons 2 4)))

   
