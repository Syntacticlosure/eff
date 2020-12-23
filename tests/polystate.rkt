#lang typed/racket
(require "../main.rkt")
(require typed/rackunit)

;; polymorphic effects
(define-effect (State ta)
  (get) : ta
  (set-state ta) : Void)

(define #:∀ (a) (add1 [tag : (Tagof (State-Freer a Integer))])
  (define x (use-State tag (get)))
  (use-State tag (set-state (+ x 1)))
  x)

(define #:∀ (a) (2times [tag : (Tagof (State-Freer a Integer))])
  (define x (use-State tag (get)))
  (use-State tag (set-state (* 2 x))))

(define #:∀ (a) (wrap [tag : (Tagof (State-Freer a String))])
  (define x (use-State tag (get)))
  (use-State tag (set-state (string-append "(" x ")"))))

(define ret-and-state
  (State-handler (Integer) : (-> Integer (Pairof Integer Integer))
                 [val : Integer (lambda ([s : Integer]) (cons val s))]
                 [(get) k (lambda ([s : Integer]) ((k s) s))]
                 [(set-state _s) k (lambda ([s : Integer]) ((k (void)) _s))]))

(define state-string
  (State-handler (String) : (-> String String)
                 [val : Void (lambda ([s : String]) s)]
                 [(get) k (lambda ([s : String]) ((k s) s))]
                 [(set-state _s) k (lambda ([s : String]) ((k (void)) _s))]))


(define a (handle-State (λ ([tag : (Tagof (State-Freer Integer Integer))])
                          (add1 tag) (add1 tag)) ret-and-state))
(define b (handle-State (λ ([tag : (Tagof (State-Freer Integer Integer))])
                          (2times tag) (add1 tag)) ret-and-state))
(define c (handle-State (λ ([tag : (Tagof (State-Freer Void String))])
                          (wrap tag)) state-string))
(module+ test
  (check-equal? (a 3) '(4 . 5))
  (check-equal? (b 3) '(6 . 7))
  (check-equal? (c "hello world") "(hello world)"))
  
