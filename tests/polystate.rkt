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

(define-effect-handler #:forall (a)
  ([ret-and-state : (State Integer)] a) : (-> Integer (Pairof a Integer))
  [val (lambda ([s : Integer]) (cons val s))]
  [(get) k (lambda ([s : Integer]) ((k s) s))]
  [(set-state _s) k (lambda ([s : Integer]) ((k (void)) _s))])

(define-effect-handler ([state-string : (State String)] Void)
  : (-> String String)
  [val (lambda ([s : String]) s)]
  [(get) k (lambda ([s : String]) ((k s) s))]
  [(set-state _s) k (lambda ([s : String]) ((k (void)) _s))])


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
  
