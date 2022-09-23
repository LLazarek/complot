#lang at-exp racket

(provide if-auto
         sender)
(require syntax/parse/define)

(define (if-auto v auto-value)
  (match v
    ['auto auto-value]
    [other other]))

(define-simple-macro (sender method args ...)
  (λ (o) (send o method args ...)))
