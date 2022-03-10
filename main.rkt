#lang at-exp racket

;; Provide all the definitions of complot, plus those from racket for when
;; complot is used as a #lang
(provide (all-from-out "complot.rkt")
         (all-from-out racket))
(require "complot.rkt")

;; This submodule is a terse way to implement racket's #lang protocol
;; to support #lang complot
(module reader syntax/module-reader complot)
