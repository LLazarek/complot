#lang at-exp racket

#|
Choices:
- support plotting multiple different variables on the same plot, sharing an axis?
  + if yes, want sth like plot1
  + if no, want sth like plot2

Examples:

---------- design 1 ----------
(plot1 (line data #:x x-col #:y y-col #:color "red")
       (points data #:x x-col #:y y-col #:color "purple")
       (point-label 55 70 "foobar")
       (x-axis #:min 0 #:ticks 10 #:label x-col) ; perhaps kind of annoying, but flexible
       (y-axis #:min 0 #:max 100 #:label y-col)  ; if an axis isn't given, it doesn't show up in the plot

       (line data #:x x-col #:y other-y-col)
       (y-axis #:far #t))

(plot1 (histogram data #:x col #:bins 20))


---------- design 2 ----------
(plot2 data
       #:x x-col
       #:y y-col
       (points)
       (x-axis #:min 0) ; axis name if not specified can default to `x-col`
       (y-axis #:min 0 #:max 100 #:label "the Y"))
(plot2 data
       #:x x-col
       #:y y-col
       (points)) ; this can have default x and y axes that use `x-col` and `y-col`
(plot2 data
       #:x x-col
       #:y y-col
       (points)
       (no-x-axis)) ; and this can remove the default x-axis


---------- design 3 ----------
(define base-plot (plot3 data
                         #:x x-col
                         #:y y-col
                         (points))) ; produces a plot data structure
(render (remove-axis base-plot #:x #:y))
(define plot+x (set-axis base-plot #:x (x-axis #:min 0 #:label "some stuff")))
(render plot+x)
(define plot+x/lines (add-viz plot+x (lines #:color "red")))

(~> (plot3 data
           #:x x-col
           #:y y-col
           (points))
    (set-axis #:x (x-axis #:min 0 #:label "some stuff" #:ticks 20))
    (add-viz (lines #:color "red")))



I think plot1 is flexible enough to be the basis, with plot2 as a layer on top.


---------- design 4 ----------
(define canvas (plot4 data)) ; produces a plot data structure
(define base-plot (with canvas (points #:x "date" #:y "price")))
(define plot+x (with base-plot (x-axis #:min 0 #:label "some stuff")))
(render plot+x)
(define plot+x/lines (with plot+x (line #:x "date" #:y "profit" #:color "red")))



Some things to think about:
- dealing with N/As in the data
  + what does graphite do?

|#

(provide graph
         with
         (rename-out [make-x-axis        x-axis]
                     [make-y-axis        y-axis]
                     [make-legend        legend]
                     [make-point-label   point-label]
                     [make-points        points]
                     [make-line          line]
                     [make-bars          bars]
                     [make-stacked-bars  stacked-bars]
                     [make-histogram     histogram]))

(require (prefix-in plot: plot)
         (prefix-in graphite: graphite)
         syntax/parse/define
         (for-syntax racket/syntax)
         data-frame
         math/statistics)

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
              layout ; todo
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
(struct stacked-bars renderer (major-col minor-col value-col invert?))
(struct histogram renderer (col bins invert?))

(struct plot (data axes legend title renderers))

(define-simple-macro (plot-set a-plot field v)
  (struct-copy plot a-plot [field v]))
(define-simple-macro (plot-update a-plot field f v)
  #:with get-field (format-id this-syntax "plot-~a" #'field)
  (struct-copy plot a-plot [field (f v (get-field a-plot))]))

(define (graph data)
  (plot data empty #f #f empty))

(define (with a-plot . things)
  (define (with-one a-plot a-thing)
    (match a-thing
      [(? axis? axis)
       (plot-update a-plot axes snoc axis)]
      [(? legend? legend)
       (plot-set a-plot legend legend)]
      [(title text)
       (plot-set a-plot title text)]

      [(? renderer? r)
       (plot-update a-plot renderers snoc r)]))
  (match things
    [(cons a-thing more)
     (apply with (with-one a-plot a-thing) more)]
    ['() a-plot]))

(define (snoc x l) (append l (list x)))

(define (make-x-axis #:label [label #f]
                     #:ticks? [ticks? #t]
                     #:major-tick-every [major-tick-every 'auto]
                     #:minor-ticks-between-major [minor-ticks-between-major 'auto]
                     #:ensure-min-tick? [ensure-min-tick? #t]
                     #:ensure-max-tick? [ensure-max-tick? #t]
                     #:minimum-ticks [minimum-ticks empty]
                     #:tick-lines? [tick-lines? #f]
                     #:layout [layout 'linear]
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
                     #:layout [layout 'linear]
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
                           #:invert? [invert? #f])
  (stacked-bars (appearance colors alpha 'auto 'auto) major minor value invert?))
(define-maker-with-appearance (make-histogram #:x x
                                              #:bins [bins 30]
                                              #:invert? [invert? #f])
  (histogram x bins invert?))


(define (render a-plot)
  (match-define (plot data axes legend title renderers) a-plot)
  (match-define (list x-min x-max x-axis-plot:renderers)
    (or (axes->plot:x-axis axes data renderers)
        (list #f #f empty)))
  (match-define (list y-min y-max y-axis-plot:renderers)
    (or (axes->plot:y-axis axes data renderers)
        (list #f #f empty)))
  (define plot:renderers (renderers->plot:renderer-tree data
                                                        renderers
                                                        #:bar-x-ticks? (not (empty? x-axis-plot:renderers))
                                                        #:bar-y-ticks? (not (empty? y-axis-plot:renderers))))
  (parameterize ([plot:plot-title title]
                 [plot:plot-x-label (axes->x-label axes)]
                 [plot:plot-y-label (axes->y-label axes)]
                 [plot:plot-x-ticks plot:no-ticks]
                 [plot:plot-y-ticks plot:no-ticks])
    (plot:plot (append (list x-axis-plot:renderers
                             y-axis-plot:renderers)
                       plot:renderers)
               #:x-min (or x-min (and (empty? plot:renderers) 0))
               #:x-max (or x-max (and (empty? plot:renderers) 0))
               #:y-min (or y-min (and (empty? plot:renderers) 0))
               #:y-max (or y-max (and (empty? plot:renderers) 0)))))

(define (renderer->plot:renderer-tree data renderer
                                      #:bar-x-ticks? bar-x-ticks?
                                      #:bar-y-ticks? bar-y-ticks?)
  (match renderer
    [(point-label (appearance color alpha size type) x y content anchor)
     (plot:point-label (renderer->plot:data data renderer)
                       content
                       #:color (if-auto color (plot:plot-foreground))
                       #:alpha (if-auto alpha (plot:label-alpha))
                       #:point-size (if-auto size (plot:label-point-size))
                       #:point-sym (if-auto type 'fullcircle)
                       #:anchor anchor)]
    [(points (appearance color alpha size type) x-col y-col)
     (plot:points (renderer->plot:data data renderer)
                  #:color (if-auto color (plot:point-color))
                  #:alpha (if-auto alpha (plot:point-alpha))
                  #:size (if-auto size (plot:point-size))
                  #:sym (if-auto type (plot:point-sym)))]
    [(line (appearance color alpha size type) x-col y-col)
     (plot:lines (renderer->plot:data data renderer)
                 #:color (if-auto color (plot:line-color))
                 #:alpha (if-auto alpha (plot:line-alpha))
                 #:width (if-auto size (plot:line-width))
                 #:style (if-auto type (plot:line-style)))]
    [(bars (appearance color alpha size type) x-col y-col)
     (plot:discrete-histogram (renderer->plot:data data renderer)
                              #:color (if-auto color (plot:rectangle-color))
                              #:alpha (if-auto alpha (plot:rectangle-alpha))
                              #:line-width (if-auto size (plot:rectangle-line-width))
                              #:style (if-auto type (plot:rectangle-style))
                              #:invert? (categorical? data y-col)
                              #:add-ticks? (if (categorical? data y-col)
                                               bar-y-ticks?
                                               bar-x-ticks?))]
    [(stacked-bars (appearance color alpha size type) major-col minor-col value-col invert?)
     (plot:stacked-histogram (renderer->plot:data data renderer)
                             #:colors (if-auto color (plot:stacked-histogram-colors))
                             #:alphas (if-auto alpha (plot:stacked-histogram-colors))
                             #:line-widths (if-auto size (plot:stacked-histogram-line-widths))
                             #:styles (if-auto type (plot:stacked-histogram-styles))
                             #:invert? invert?
                             #:add-ticks? (if invert?
                                              bar-y-ticks?
                                              bar-x-ticks?))]
    [(histogram (appearance color alpha size type) x-col bins invert?)
     (plot:discrete-histogram (renderer->plot:data data renderer)
                              #:color (if-auto color (plot:rectangle-color))
                              #:alpha (if-auto alpha (plot:rectangle-alpha))
                              #:line-width (if-auto size (plot:rectangle-line-width))
                              #:style (if-auto type (plot:rectangle-style))
                              #:invert? invert?
                              #:add-ticks? (if invert?
                                               bar-y-ticks?
                                               bar-x-ticks?))]))
(define (renderers->plot:renderer-tree data renderers
                                       #:bar-x-ticks? bar-x-ticks?
                                       #:bar-y-ticks? bar-y-ticks?)
  (let loop ([renderers renderers])
    (match renderers
      [(cons r more)
       (cons (renderer->plot:renderer-tree data r
                                           #:bar-x-ticks? bar-x-ticks?
                                           #:bar-y-ticks? bar-y-ticks?)
             (loop more))]
      ['() empty])))

(define (categorical? data col)
  (match (df-select data col)
    [(vector (? real?) ...) #f]
    [else #t]))

(define-simple-macro (define-axes-> field:id {~optional extractor})
  #:with axes->x (format-id this-syntax "axes->x-~a" #'field)
  #:with axes->y (format-id this-syntax "axes->y-~a" #'field)
  #:with axes->  (format-id #f "axes->~a" #'field)
  #:with getter  (format-id this-syntax "axis-~a" #'field)
  (begin
    (define (axes->x axes) (axes-> x-axis? axes))
    (define (axes->y axes) (axes-> y-axis? axes))
    (define (axes-> right-axis? axes)
      (for/first ([an-axis (in-list axes)]
                  #:when (right-axis? an-axis))
        {~? (extractor an-axis) (getter an-axis)}))))
(define-axes-> label)
(define-axes-> min)
(define-axes-> max)

(define (axes->plot:x-axis axes data renderers)
  (axes->plot:axis x-axis?
          plot:x-ticks
          axes
          data
          renderers))
(define (axes->plot:y-axis axes data renderers)
  (axes->plot:axis y-axis?
          plot:y-ticks
          axes
          data
          renderers))
(define (axes->plot:axis right-axis? make-plot:axis axes data renderers)
  (for/first ([an-axis (in-list axes)]
              #:when (right-axis? an-axis))
    (define-values {the-min the-max categorical?}
      (cond [(and (axis-min an-axis)
                  (axis-max an-axis))
             ;; Avoid calculating any inferred bounds if not needed
             (values (axis-min an-axis) (axis-max an-axis) #f)]
            [else
             (define-values {inferred-min inferred-max categorical?}
               (infer-bounds data renderers (x-axis? an-axis)))
             (values (or (axis-min an-axis) inferred-min)
                     (or (axis-max an-axis) inferred-max)
                     categorical?)]))
    (define extra-ticks
      (append (if (axis-ensure-min-tick? an-axis)
                  (list the-min)
                  empty)
              (if (axis-ensure-max-tick? an-axis)
                  (list the-max)
                  empty)
              (axis-minimum-ticks an-axis)))
    (list the-min
          the-max
          (list (make-plot:axis
                 (if (and (axis-ticks? an-axis)
                          (not categorical?))
                     (plot:ticks-generate
                      (plot:ticks-add
                       (match* {(axis-major-ticks-every an-axis)
                                (axis-minor-ticks-between-major an-axis)}
                         [{'auto _} (plot:linear-ticks)]
                         [{(or #f 0) _} (plot:ticks plot:no-ticks-layout
                                                    (plot:linear-ticks-format))]
                         [{n 0}
                          (plot:linear-ticks
                           #:number (max (quotient (- the-max the-min) n) 1)
                           #:divisors '(1))]
                         [{n 'auto}
                          (plot:linear-ticks
                           #:number (max (quotient (- the-max the-min) n) 1))])
                       extra-ticks)
                      the-min
                      the-max)
                     ;; either categorical? is true, so we need the axis, but since it's categorical we leave it up to plot's internal handling above in render
                     ;; otherwise we don't need the ticks at all
                     empty))
                (if (axis-tick-lines? an-axis)
                    (if (x-axis? an-axis)
                        (plot:x-tick-lines)
                        (plot:y-tick-lines))
                    empty)))))

(define (if-auto v auto-value)
  (match v
    ['auto auto-value]
    [other other]))

(define (renderer->plot:data data renderer)
  (match renderer
    [(point-label _ x y _ _)
     (list x y)]
    [(or (points _ (? string? x-col) (? string? y-col))
         (line _ (? string? x-col) (? string? y-col))
         (bars _ (? string? x-col) (? string? y-col)))
     (vector->list
      (vector-map vector->list
                  (df-select* data
                              x-col
                              y-col)))]
    #;[(points _ (? string? x-col) #f)
       (vector->list (df-select data x-col))]
    [(stacked-bars _ major-col minor-col value-col invert?)
     ;; todo
     #f]
    [(histogram _ col bins invert?)
     (cond [(categorical? data col)
            (define frequencies
              (samples->hash (df-select data col)))
            (hash-map frequencies list)]
           [else
            (define the-values (vector->list (df-select data col)))
            (define values-min (apply min the-values))
            (define values-max (apply max the-values))
            (define bin-bounds (range values-min
                                      values-max
                                      (/ (- values-max values-min) bins)))
            (for/list ([a-bin (bin-samples bin-bounds <= the-values)])
              (list (/ (- (sample-bin-max a-bin) (sample-bin-min a-bin)) 2)
                    (length (sample-bin-values a-bin))))])]))

(define (infer-bounds data renderers x? #|otherwise y|#)
  (define (real-min/maxes vs)
    (list (apply min vs) (apply max vs) #f))
  (define mins+maxes+categorical?
    (for/list ([renderer (in-list renderers)])
      (match (renderer->plot:data data renderer)
        [(list (list (? real? xs) _) ...)
         #:when x?
         (real-min/maxes xs)]
        [(list (list _ (? real? ys)) ...)
         #:when (not x?)
         (real-min/maxes ys)]
        [(list (list (? (negate real?) xs) _) ...)
         #:when x?
         (list 0 (length xs) #t)]
        [(list (list _ (? (negate real?) ys)) ...)
         #:when (not x?)
         (list 0 (length ys) #t)])))
  (values (apply min (map first mins+maxes+categorical?))
          (apply max (map second mins+maxes+categorical?))
          (andmap third mins+maxes+categorical?)))

(module+ test
  (require sawzall)
  (render (graph (row-df [date price]
                         1 20.50
                         2 22
                         3 20
                         4 23
                         5 26.34)))
  (render (with (graph (row-df [date price]
                               1 20.50
                               2 22
                               3 20
                               4 23
                               5 26.34))
                (make-points #:x "date" #:y "price" #:alpha 1)
                (make-x-axis #:min 0 #:max 5)
                (make-y-axis #:min 0 #:max 30)))
  (render (with (graph (row-df [date price]
                               1 20.50
                               2 22
                               3 20
                               4 23
                               5 26.34))
                (make-points #:x "date" #:y "price" #:alpha 1)
                (make-line #:x "date" #:y "date")
                (make-y-axis #:min 0 #:max 30)))
  (render (with (graph (row-df [date price ok?]
                               1 20.50 "yes"
                               2 22 "no"
                               3 20 "no"
                               4 23 "no"
                               4 23 "yes"
                               5 26.34 "kinda"))
                (make-histogram #:x "ok?")
                (make-x-axis)
                (make-y-axis)))
  (render (with (graph (row-df [date price ok?]
                               1 20.50 "yes"
                               2 22 "no"
                               3 20 "no"
                               4 23 "no"
                               4 23 "yes"
                               5 26.34 "kinda"))
                (make-histogram #:x "ok?")
                (make-x-axis)
                (make-y-axis #:min 0 #:major-tick-every 1 #:minor-ticks-between-major 0))))
