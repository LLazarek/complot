#lang at-exp racket

;; Provide bindings from racket for when complot is used as a #lang
(provide (all-from-out "complot.rkt")
         (all-from-out racket))
(require "complot.rkt")

(module reader syntax/module-reader complot)
