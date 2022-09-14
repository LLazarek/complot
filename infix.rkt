#lang at-exp racket/base

(provide (all-from-out "complot.rkt"))
(require "complot.rkt")

(module reader syntax/module-reader
  complot/lang

  ;; this copied from sweet-exp's reader submod
  #:read sweet-read
  #:read-syntax sweet-read-syntax
  #:info get-info

  (require (submod sweet-exp link-reader))
  (define-values (sweet-read sweet-read-syntax)
    (sweet-link read read-syntax))

  (define (get-info key default f)
    (define (fallback) (f key default))
    (case key
      [(drracket:indentation)
       (dynamic-require 'sweet-exp/indent 'indent)]
      [else (fallback)])))
