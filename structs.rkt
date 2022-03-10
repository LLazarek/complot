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

;; The printer parameter and its default, which gets overriden with one that
;; uses `render` in `complot.rkt`
(define current-complot-printer
  (make-parameter (λ (thing port mode)
                    ;; This @~a{...} syntax is like string interpolation in other languages
                    ;; It creates a string that can embed code and values using @
                    (display @~a{#<complot @(object-name thing)>}
                             port))))

;; The root / parent of all complot data structures, just used to attach the
;; property that tells the racket runtime to use a custom printing function.
(struct complot-printable ()
  #:methods gen:custom-write
  [(define (write-proc . args)
     ;; The custom printing function just delegates to the current value of the
     ;; printer parameter.
     (apply (current-complot-printer) args))])

;; A shared data structure declaration for holding appearance information
;; The syntax of struct declarations is
;; (struct <name> [<maybe-a-parent>] (<field-name> ...))
;; This struct has no parents.
;; Racket structs are very much like C structs in semantics
;; (albeit with some extra bells and whistles).
(struct appearance (color
                    alpha
                    size ; size for points, thickness for lines
                    type ; symbol for points, style for lines
                    label))

;; --- Plot element structs ---
;; This struct inherits from complot-printable, so gets the custom-printing property too
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
;; The x- and y-axis structs inherit from axis, getting all of its fields and properties
;; They don't add any new ones.
(struct x-axis axis ())
(struct y-axis axis ())
(struct legend (position type))
(struct title complot-printable (text))

;; --- Renderer structs ---
(struct renderer complot-printable (appearance))
(struct point-label renderer (x y content anchor))
(struct points renderer (x-col y-col facet-col))
(struct line renderer (x-col y-col))
(struct bars renderer (x-col y-col invert?))
(struct stacked-bars renderer (x-col facet-col y-col invert? aggregator labels?))
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

;; The plot constructor; this is renamed to just `plot` when exported by `complot.rkt`
(define (make-plot data)
  (plot data #f #f #f #f empty))

;; A convenience macro for defining the axis constructors, since they are identical
;; except for the name and creating an x- or y-axis
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

;; A convenience macro for defining renderer constructors, since most of them have the same
;; appearance options.
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
                                           #:y y
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

