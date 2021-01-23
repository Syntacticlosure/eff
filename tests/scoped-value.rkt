#lang racket
;; an example illustrates scoped value effects
(require "../main.rkt")
(define-value-effect log)
(define (os-code)
  (log "started.")
  (log "stopped."))

(define (library-code)
  (log "started.")
  (mask-effect (log) (os-code))
  (log "stopped."))

(define (user-code)
  (log "started.")
  (mask-effect (log) (library-code))
  (log "stopped."))

(define (run code)
  (with-effect/value
      ([log (λ (x) (printf "os level log : ~a\n" x))]
       [log (λ (x) (printf "library level log : ~a\n" x))]
       [log (λ (x) (printf "user level log: ~a\n" x))])
    (code)))

       
