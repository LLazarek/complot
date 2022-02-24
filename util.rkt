#lang at-exp racket

(provide if-auto)

(define (if-auto v auto-value)
  (match v
    ['auto auto-value]
    [other other]))
