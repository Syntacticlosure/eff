#lang typed/racket
(require "../main.rkt")
(require typed/rackunit)
;; an example to illustrate dynamic effects

(define-effect (Cell a)
  (cell-get-ref) : a
  (cell-set-ref a) : Void)
