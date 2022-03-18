#lang at-exp racket

(provide (struct-out appearance)
         (struct-out converters)
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

;; The printer parameter and its default, which gets overriden with one that
;; uses `render` in `complot.rkt`
(define current-complot-printer
  (make-parameter (λ (thing port mode)
                    ;; This @~a{...} syntax is like string interpolation in other languages
                    ;; It creates a string that can embed code and values using @
                    (display @~a{#<complot @(object-name thing)>}
                             port))))

;; The root / parent of all complot data structures, just used to attach this
;; custom-write method...
(struct complot-printable ()
  #:methods gen:custom-write
  [(define (write-proc . args)
     ;; ... which just delegates to the current value of the printer parameter.
     (apply (current-complot-printer) args))])

(struct appearance (color
                    alpha
                    size ; size for points, thickness for lines
                    type ; symbol for points, style for lines
                    label))

(struct converters (x y group))

;; --- Plot element structs ---
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

;; --- Renderer structs ---
(struct renderer complot-printable (appearance converters))
(struct point-label renderer (x y content anchor))
(struct points renderer (x-col y-col group-col))
(struct line renderer (x-col y-col))
(struct bars renderer (x-col y-col invert?))
(struct stacked-bars renderer (x-col group-col y-col invert? aggregator labels?))
(struct histogram renderer (col bins invert?))
(struct function renderer (f min max))

;; --- The plot struct ---
(struct plot complot-printable (data x-axis y-axis legend title renderers))

;; Some convenience macros
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

(define-simple-macro (define-simple-renderer (id:id formals ...)
                       (s e ...))
  (define (id formals ...
              #:color [color 'auto]
              #:alpha [alpha 1]
              #:size [size 'auto]
              #:type [type 'auto]
              #:label [label 'auto]

              #:x-converter [x-converter #f]
              #:y-converter [y-converter #f]
              #:group-converter [group-converter #f])
    (s (appearance color alpha size type label)
       (converters x-converter y-converter group-converter)
       e ...)))
(define-simple-renderer (make-point-label x y content
                                          #:anchor [anchor 'auto])
  (point-label x y content anchor))
(define-simple-renderer (make-points #:x x
                                     #:y y
                                     #:group-by [group-col #f]) ;; todo: support dot plots
  (points x y group-col))
(define-simple-renderer (make-line #:x x
                                   #:y y)
  (line x y))
(define-simple-renderer (make-bars #:x x
                                   #:y y
                                   #:invert? [invert? #f])
  (bars x y invert?))
(define (make-stacked-bars #:x x-col
                           #:group-by group-col
                           #:y y-col
                           #:colors [colors 'auto]
                           #:alpha [alpha 1]
                           #:invert? [invert? #f]
                           #:aggregate [aggregator +]
                           #:labels? [labels? #t]

                           #:x-converter [x-converter #f]
                           #:y-converter [y-converter #f]
                           #:group-converter [group-converter #f])
  (stacked-bars (appearance colors alpha 'auto 'auto 'auto)
                (converters x-converter y-converter group-converter)
                x-col
                group-col
                y-col
                invert?
                aggregator
                labels?))
(define-simple-renderer (make-histogram #:x x
                                        #:bins [bins 30]
                                        #:invert? [invert? #f])
  (histogram x bins invert?))
(define-simple-renderer (make-function f
                                       #:min min
                                       #:max max)
  (function f min max))

