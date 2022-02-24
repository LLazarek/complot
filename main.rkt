#lang at-exp racket

(provide (all-from-out "complot.rkt")
         (all-from-out racket))
(require "complot.rkt")

(module reader syntax/module-reader complot)
