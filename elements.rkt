#lang at-exp racket

(provide make-x-axis
         make-y-axis
         make-legend
         (struct-out axis)
         (struct-out x-axis)
         (struct-out y-axis)
         (struct-out legend)
         (struct-out title))

(require "basics.rkt"
         syntax/parse/define)

(struct axis complot-printable (label
                                ticks?
                                major-ticks-every
                                minor-ticks-between-major
                                tick-lines?
                                min
                                max
                                layout
                                ensure-min-tick?
                                ensure-max-tick?
                                minimum-ticks))
(struct x-axis axis ())
(struct y-axis axis ())
(struct legend (position type))
(struct title complot-printable (text))

(define-simple-macro (define-axis-maker name axis)
  (define (name #:label [label #f]
                #:ticks? [ticks? #t]
                #:major-tick-every [major-tick-every 'auto]
                #:minor-ticks-between-major [minor-ticks-between-major 'auto]
                #:ensure-min-tick? [ensure-min-tick? #t]
                #:ensure-max-tick? [ensure-max-tick? #t]
                #:minimum-ticks [minimum-ticks empty]
                #:tick-lines? [tick-lines? #f]
                #:layout [layout 'auto]
                #:min [min #f]
                #:max [max #f])
    (axis label
          ticks?
          major-tick-every
          minor-ticks-between-major
          tick-lines?
          min
          max
          layout
          ensure-min-tick?
          ensure-max-tick?
          minimum-ticks)))
(define-axis-maker make-x-axis x-axis)
(define-axis-maker make-y-axis y-axis)

(define (make-legend #:position [position 'auto]
                     #:type [type 'new])
  (legend position type))

