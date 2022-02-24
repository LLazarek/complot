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

         graph
         make-x-axis
         make-y-axis
         make-legend
         make-point-label
         make-points
         make-line
         make-bars
         make-stacked-bars
         make-histogram
         make-function)

(require syntax/parse/define
         (for-syntax racket/syntax))

(struct appearance (color
                    alpha
                    size ; size for points, thickness for lines
                    type ; symbol for points, style for lines
                    ))

(struct axis (label
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
(struct legend (position))
(struct title (text))

(struct renderer (appearance))
(struct point-label renderer (x y content anchor))
(struct points renderer (x-col y-col))
(struct line renderer (x-col y-col))
(struct bars renderer (x-col y-col))
(struct stacked-bars renderer (major-col minor-col value-col invert? aggregator labels?))
(struct histogram renderer (col bins invert?))
(struct function renderer (f min max name))

(struct plot (data x-axis y-axis legend title renderers))

(define-simple-macro (plot-set a-plot field v)
  (struct-copy plot a-plot [field v]))
(define-simple-macro (plot-update a-plot field f v)
  #:with get-field (format-id this-syntax "plot-~a" #'field)
  (struct-copy plot a-plot [field (f v (get-field a-plot))]))

(define (graph data)
  (plot data #f #f #f #f empty))

(define (make-x-axis #:label [label #f]
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
  (x-axis label
          ticks?
          major-tick-every
          minor-ticks-between-major
          tick-lines?
          min
          max
          layout
          ensure-min-tick?
          ensure-max-tick?
          minimum-ticks))
(define (make-y-axis #:label [label #f]
                     #:ticks [ticks? #t]
                     #:major-tick-every [major-tick-every 'auto]
                     #:minor-ticks-between-major [minor-ticks-between-major 'auto]
                     #:ensure-min-tick? [ensure-min-tick? #t]
                     #:ensure-max-tick? [ensure-max-tick? #t]
                     #:minimum-ticks [minimum-ticks empty]
                     #:tick-lines? [tick-lines? #f]
                     #:layout [layout 'auto]
                     #:min [min #f]
                     #:max [max #f])
  (y-axis label
          ticks?
          major-tick-every
          minor-ticks-between-major
          tick-lines?
          min
          max
          layout
          ensure-min-tick?
          ensure-max-tick?
          minimum-ticks))
(define (make-legend #:position [position 'auto])
  (legend position))

(define-simple-macro (define-maker-with-appearance (id:id formals ...)
                       (s e ...))
  (define (id formals ...
              #:color [color 'auto]
              #:alpha [alpha 1]
              #:size [size 'auto]
              #:type [type 'auto])
    (s (appearance color alpha size type) e ...)))
(define-maker-with-appearance (make-point-label x y content
                                                #:anchor [anchor 'auto])
  (point-label x y content anchor))
(define-maker-with-appearance (make-points #:x x
                                           #:y [y #f])
  (points x y))
(define-maker-with-appearance (make-line #:x x
                                         #:y y)
  (line x y))
(define-maker-with-appearance (make-bars #:x x
                                         #:y y)
  (bars x y))
(define (make-stacked-bars #:category major
                           #:subcategory minor
                           #:value value
                           #:colors [colors 'auto]
                           #:alpha [alpha 1]
                           #:invert? [invert? #f]
                           #:aggregate [aggregator +]
                           #:labels? [labels? #t])
  (stacked-bars (appearance colors alpha 'auto 'auto) major minor value invert? aggregator labels?))
(define-maker-with-appearance (make-histogram #:x x
                                              #:bins [bins 30]
                                              #:invert? [invert? #f])
  (histogram x bins invert?))
(define-maker-with-appearance (make-function f
                                             #:min min
                                             #:max max
                                             #:name [name #f])
  (function f min max name))

