#lang typed/racket
(require "../main.rkt")
(require typed/rackunit)

;; polymorphic effects
(define-effect (State a)
  (get) : a
  (set-state a) : Void)
(define tag (make-State-tag Integer))
(define (add1)
  (define x (use-State tag (get)))
  (use-State tag (set-state (+ x 1)))
  x)

(define (2times)
  (define x (use-State tag (get)))
  (use-State tag (set-state (* 2 x))))

(define string-tag (make-State-tag String))
(define (wrap)
  (define x (use-State string-tag (get)))
  (use-State string-tag (set-state (string-append "(" x ")"))))

(define ret-and-state
  (effect-handler
   #:forall (a b) (State a) : (-> a (Pairof b a))
   [val : b (lambda ([s : a]) (cons val s))]
   [(get) k (lambda ([s : a]) ((k s) s))]
   [(set-state _s) k (lambda ([s : a]) ((k (void)) _s))]))

(define state-string
  (effect-handler
   (State String) : (-> String String)
   [val : Void (lambda ([s : String]) s)]
   [(get) k (lambda ([s : String]) ((k s) s))]
   [(set-state _s) k (lambda ([s : String]) ((k (void)) _s))]))


(define a (ret-and-state tag (thunk
                          (add1) (add1))))
(define b (ret-and-state tag (thunk
                          (2times) (add1))))
(define c (state-string string-tag (thunk
                          (wrap))))
(module+ test
  (check-equal? (a 3) '(4 . 5))
  (check-equal? (b 3) '(6 . 7))
  (check-equal? (c "hello world") "(hello world)"))
  
