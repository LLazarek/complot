#lang at-exp racket

(provide (struct-out appearance)
         (struct-out axis)
         (struct-out x-axis)
         (struct-out y-axis)
         (struct-out legend)
         (struct-out title)
         (struct-out renderer)
         (struct-out point-label)
         (struct-out points)
         (struct-out line)
         (struct-out bars)
         (struct-out stacked-bars)
         (struct-out histogram)
         (struct-out function)
         (struct-out plot)

         plot-set
         plot-update

         make-plot
         make-x-axis
         make-y-axis
         make-legend
         make-point-label
         make-points
         make-line
         make-bars
         make-stacked-bars
         make-histogram
         make-function

         current-complot-printer)

(require syntax/parse/define
         (for-syntax racket/syntax))

(define current-complot-printer (make-parameter (λ (thing port mode)
                                                  (display @~a{#<complot @(object-name thing)>}
                                                           port))))

(struct appearance (color
                    alpha
                    size ; size for points, thickness for lines
                    type ; symbol for points, style for lines
                    label
                    ))

(struct complot-printable ()
  #:methods gen:custom-write
  [(define (write-proc . args)
     (apply (current-complot-printer) args))])

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

(struct renderer complot-printable (appearance))
(struct point-label renderer (x y content anchor))
(struct points renderer (x-col y-col facet-col))
(struct line renderer (x-col y-col))
(struct bars renderer (x-col y-col invert?))
(struct stacked-bars renderer (x-col facet-col y-col invert? aggregator labels?))
(struct histogram renderer (col bins invert?))
(struct function renderer (f min max))

(struct plot complot-printable (data x-axis y-axis legend title renderers))

(define-simple-macro (plot-set a-plot field v)
  (struct-copy plot a-plot [field v]))
(define-simple-macro (plot-update a-plot field f v)
  #:with get-field (format-id this-syntax "plot-~a" #'field)
  (struct-copy plot a-plot [field (f v (get-field a-plot))]))

(define (make-plot data)
  (plot data #f #f #f #f empty))

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

(define-simple-macro (define-maker-with-appearance (id:id formals ...)
                       (s e ...))
  (define (id formals ...
              #:color [color 'auto]
              #:alpha [alpha 1]
              #:size [size 'auto]
              #:type [type 'auto]
              #:label [label 'auto])
    (s (appearance color alpha size type label) e ...)))
(define-maker-with-appearance (make-point-label x y content
                                                #:anchor [anchor 'auto])
  (point-label x y content anchor))
(define-maker-with-appearance (make-points #:x x
                                           #:y [y #f]
                                           #:facet [facet #f]) ;; todo: support dot plots
  (points x y facet))
(define-maker-with-appearance (make-line #:x x
                                         #:y y)
  (line x y))
(define-maker-with-appearance (make-bars #:x x
                                         #:y y
                                         #:invert? [invert? #f])
  (bars x y invert?))
(define (make-stacked-bars #:x x-col
                           #:facet facet-col
                           #:y y-col
                           #:colors [colors 'auto]
                           #:alpha [alpha 1]
                           #:invert? [invert? #f]
                           #:aggregate [aggregator +]
                           #:labels? [labels? #t])
  (stacked-bars (appearance colors alpha 'auto 'auto 'auto)
                x-col
                facet-col
                y-col
                invert?
                aggregator
                labels?))
(define-maker-with-appearance (make-histogram #:x x
                                              #:bins [bins 30]
                                              #:invert? [invert? #f])
  (histogram x bins invert?))
(define-maker-with-appearance (make-function f
                                             #:min min
                                             #:max max)
  (function f min max))

