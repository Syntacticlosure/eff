#lang typed/racket

(provide Tagof TagHandler
         call/prompt call/comp abort-cc
         call/reset call/shift
         Pure)

;; type defs for prompt tags
(define-type (TagHandler a) (-> (-> a) a))
(define-type (Tagof a) (Prompt-Tagof a (TagHandler a)))

;; abrevs
(define call/prompt call-with-continuation-prompt)
(define call/comp call-with-composable-continuation)
(define abort-cc abort-current-continuation)

;; Freer Monad
(struct Pure ([value : Any]))

;; definitions for reset and shift
;; thanks jqww6 for help
(: call/reset (All (a) (-> (-> a) (Tagof a) a)))
(define (call/reset body-thunk tag)
  (call/prompt body-thunk
               tag))

(: call/shift (All (a b ...) (-> (-> (-> b ... b a) a)
                                 (Tagof a) (Values b ... b))))
(define (call/shift body-thunk tag)
  (call/comp (lambda ([k : (-> b ... b a)])
               (abort-cc tag
                         (thunk (body-thunk
                                 (lambda xs
                                   (call/reset (thunk (apply k xs))
                                               tag))))))
             tag))

(define test-tag : (Tagof Integer) (make-continuation-prompt-tag))

