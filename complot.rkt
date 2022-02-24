#lang at-exp racket

#|
todo
- ✓ Support stacked bar graphs
- ✓ Support legends
- ✓ Support axis transformations (e.g. log scale)
- Support displaying sub-pieces of a graph (e.g. an axis by itself)
- Add data primitives
- Add a primitive for saving plots to file
- Start the gui
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
                     [make-histogram     histogram]
                     [make-function      function]))

(require (prefix-in plot: plot)
         (prefix-in graphite: graphite)
         sawzall
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

(define (with a-plot . things)
  (define (with-one a-plot a-thing)
    (match a-thing
      [(? x-axis? axis)
       (plot-set a-plot x-axis axis)]
      [(? y-axis? axis)
       (plot-set a-plot y-axis axis)]
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


(define (render a-plot)
  (match-define (plot data x-axis y-axis legend title renderers) a-plot)
  (match-define (list x-min x-max x-axis-plot:renderers)
    (if x-axis
        (x-axis->plot:axis x-axis data renderers)
        (list #f #f empty)))
  (match-define (list y-min y-max y-axis-plot:renderers)
    (if y-axis
        (y-axis->plot:axis y-axis data renderers)
        (list #f #f empty)))
  (define plot:renderers (renderers->plot:renderer-tree data
                                                        renderers
                                                        #:bar-x-ticks? (not (empty? x-axis-plot:renderers))
                                                        #:bar-y-ticks? (not (empty? y-axis-plot:renderers))
                                                        #:legend? legend))
  (parameterize ([plot:plot-title title]
                 [plot:plot-x-label (axis->label x-axis)]
                 [plot:plot-y-label (axis->label y-axis)]
                 [plot:plot-x-ticks plot:no-ticks]
                 [plot:plot-y-ticks plot:no-ticks]
                 [plot:plot-x-transform (first (axis->ticks+transform x-axis))]
                 [plot:plot-y-transform (first (axis->ticks+transform y-axis))])
    (plot:plot (append (list x-axis-plot:renderers
                             y-axis-plot:renderers)
                       plot:renderers)
               #:x-min (or x-min (and (empty? plot:renderers) 0))
               #:x-max (or x-max (and (empty? plot:renderers) 0))
               #:y-min (or y-min (and (empty? plot:renderers) 0))
               #:y-max (or y-max (and (empty? plot:renderers) 0))
               #:legend-anchor (if legend
                                   (if-auto (legend-position legend)
                                            (plot:plot-legend-anchor))
                                   (plot:plot-legend-anchor)))))

(define (renderer->plot:renderer-tree data renderer
                                      #:bar-x-ticks? bar-x-ticks?
                                      #:bar-y-ticks? bar-y-ticks?
                                      #:legend? add-legend?)
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
                  #:sym (if-auto type (plot:point-sym))
                  #:label (and add-legend? y-col))]
    [(line (appearance color alpha size type) x-col y-col)
     (plot:lines (renderer->plot:data data renderer)
                 #:color (if-auto color (plot:line-color))
                 #:alpha (if-auto alpha (plot:line-alpha))
                 #:width (if-auto size (plot:line-width))
                 #:style (if-auto type (plot:line-style))
                 #:label (and add-legend? y-col))]
    [(bars (appearance color alpha size type) x-col y-col)
     (plot:discrete-histogram (renderer->plot:data data renderer)
                              #:color (if-auto color (plot:rectangle-color))
                              #:alpha (if-auto alpha (plot:rectangle-alpha))
                              #:line-width (if-auto size (plot:rectangle-line-width))
                              #:style (if-auto type (plot:rectangle-style))
                              #:invert? (categorical? data y-col)
                              #:add-ticks? (if (categorical? data y-col)
                                               bar-y-ticks?
                                               bar-x-ticks?)
                              #:label (and add-legend? y-col))]
    [(stacked-bars (appearance color alpha size type) major-col minor-col value-col invert? _ labels?)
     (define raw-data (renderer->plot:data data renderer))
     (list (plot:stacked-histogram raw-data
                                   #:colors (if-auto color (plot:stacked-histogram-colors))
                                   #:alphas (list (if-auto alpha (plot:stacked-histogram-alphas)))
                                   #:line-widths (if-auto size (plot:stacked-histogram-line-widths))
                                   #:styles (if-auto type (plot:stacked-histogram-styles))
                                   #:invert? invert?
                                   #:add-ticks? (if invert?
                                                    bar-y-ticks?
                                                    bar-x-ticks?)
                                   ;; #:labels (if legend?
                                   ;;              ...
                                   ;;              '(#f))
                                   )
           (if labels?
               (make-stacked-bar-labels data
                                        raw-data
                                        major-col
                                        minor-col)
               empty))]
    [(histogram (appearance color alpha size type) x-col bins invert?)
     (plot:discrete-histogram (renderer->plot:data data renderer)
                              #:color (if-auto color (plot:rectangle-color))
                              #:alpha (if-auto alpha (plot:rectangle-alpha))
                              #:line-width (if-auto size (plot:rectangle-line-width))
                              #:style (if-auto type (plot:rectangle-style))
                              #:invert? invert?
                              #:add-ticks? (if invert?
                                               bar-y-ticks?
                                               bar-x-ticks?)
                              #:label (and add-legend? (~a x-col " count")))]
    [(function (appearance color alpha size type) f min max name)
     (plot:function f
                    min
                    max
                    #:color (if-auto color (plot:line-color))
                    #:alpha (if-auto alpha (plot:line-alpha))
                    #:width (if-auto size (plot:line-width))
                    #:style (if-auto type (plot:line-style))
                    #:label (and add-legend? name))]
    [(? add-legend?) empty]))
(define (renderers->plot:renderer-tree data renderers
                                       #:bar-x-ticks? bar-x-ticks?
                                       #:bar-y-ticks? bar-y-ticks?
                                       #:legend? add-legend?)
  (let loop ([renderers renderers])
    (match renderers
      [(cons r more)
       (cons (renderer->plot:renderer-tree data r
                                           #:bar-x-ticks? bar-x-ticks?
                                           #:bar-y-ticks? bar-y-ticks?
                                           #:legend? add-legend?)
             (loop more))]
      ['() empty])))

(define (categorical? data col)
  (match (df-select data col)
    [(vector (? real?) ...) #f]
    [else #t]))

(define (axis->label maybe-axis)
  (if (axis? maybe-axis)
      (axis-label maybe-axis)
      #f))

(define (x-axis->plot:axis x-axis data renderers)
  (axis->plot:axis x-axis
                   plot:x-ticks
                   data
                   renderers))
(define (y-axis->plot:axis y-axis data renderers)
  (axis->plot:axis y-axis
                   plot:y-ticks
                   data
                   renderers))
(define (axis->plot:axis an-axis make-plot:axis data renderers)
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
  (define plot:ticks-fn (second (axis->ticks+transform an-axis)))
  (list the-min
        the-max
        (list (make-plot:axis
               (if (and (axis-ticks? an-axis)
                        (not categorical?))
                   (plot:ticks-generate
                    (plot:ticks-add
                     (match* {(axis-major-ticks-every an-axis)
                              (axis-minor-ticks-between-major an-axis)}
                       [{'auto _} plot:ticks-fn]
                       [{(or #f 0) _} (plot:ticks plot:no-ticks-layout
                                                  (plot:linear-ticks-format))]
                       [{n _}
                        (=> continue)
                        (match (axis-layout an-axis)
                          [(or 'auto 'linear) (continue)]
                          [other (raise-user-error
                                  'complot
                                  @~a{
                                      #:major-tick-every n is only supported
                                      for linear axis layouts (the default),
                                      but you asked for a non-linear layout:
                                      @~s[other]
                                      })])]
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
                  empty))))

(define (axis->ticks+transform axis)
  (match (and axis (axis-layout axis))
    [(or #f 'auto) (list (plot:plot-x-transform) (plot:plot-x-ticks))]
    ['linear (list plot:id-transform (plot:linear-ticks))]
    ['log (list plot:log-transform (plot:log-ticks))]
    [other other]))

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
    [(stacked-bars _ major-col minor-col value-col invert? aggregator _)
     #|
     | group    | category  | money |
     |----------+-----------+-------|
     | expenses | food      |    20 |
     |          | transport |    30 |
     |          | laundry   |    10 |
     | income   | paycheck  |   100 |
     |          | side-job  |    10 |
        ^          ^           ^
       major      minor       value
     |#
     (define sorted-data (sort-data-by-major/minor-groups data
                                                          major-col
                                                          minor-col))
     (for/list ([major-col-group-df (in-list (split-with sorted-data major-col))])
       (define group-value (vector-ref (df-select major-col-group-df major-col) 0))
       (list group-value
             (for/list ([minor-col-group-df (in-list (split-with major-col-group-df minor-col))])
               (apply aggregator (vector->list (df-select minor-col-group-df value-col))))))]
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
                    (length (sample-bin-values a-bin))))])]
    [(function _ f min max _)
     (for/list ([x (in-range min max (/ (- max min) (plot:line-samples)))])
       (list x (f x)))]))

(define (sort-data-by-major/minor-groups data major-col minor-col)
  (define major-sorted (reorder data major-col))
  (define grouped-by-major (group-with major-sorted major-col))
  (define sorted-within-major (reorder grouped-by-major minor-col))
  (ungroup sorted-within-major))

(define (data->stacked-bar-label-info data
                                      raw-data
                                      major-col
                                      minor-col)
  (define sorted-data (sort-data-by-major/minor-groups data
                                                       major-col
                                                       minor-col))
  (append*
   (for/list ([major-col-group-df (in-list (split-with sorted-data major-col))]
              [major-col-group    (in-list raw-data)]
              [x-pos              (in-range 0.5 (+ 0.5 (length raw-data)))])
     (for/fold ([points empty]
                [bar-height-so-far 0]
                #:result points)
               ([minor-col-group-df (in-list (split-with major-col-group-df minor-col))]
                [minor-col-value    (in-list (second major-col-group))])
       (define minor-group-key (vector-ref (df-select minor-col-group-df minor-col) 0))
       (values (cons (list x-pos
                           (+ bar-height-so-far (/ minor-col-value 2))
                           minor-group-key)
                     points)
               (+ bar-height-so-far minor-col-value))))))
(define (make-stacked-bar-labels data
                                 raw-data
                                 major-col
                                 minor-col)
  (for/list ([info (in-list (data->stacked-bar-label-info data raw-data major-col minor-col))])
    (match-define (list x-pos y-pos label) info)
    (plot:point-label (list x-pos y-pos)
                      label
                      #:anchor 'center
                      #:point-size 0)))

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
        [(list (list _ (list (? real? y-part-lists) ...)) ...)
         #:when (not x?)
         (define y-totals
           (for/list ([y-parts (in-list y-part-lists)])
             (apply + y-parts)))
         (real-min/maxes y-totals)]
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
                (make-y-axis #:min 0 #:major-tick-every 1 #:minor-ticks-between-major 0)))
  (render (with (graph (row-df [date price ok?]
                               1 20.50 "yes"
                               2 22 "no"
                               3 20 "no"
                               4 23 "no"
                               4 23 "yes"
                               5 26.34 "kinda"))
                (make-histogram #:x "ok?")
                (make-x-axis)
                (make-y-axis #:min 0 #:major-tick-every #f
                             #:minimum-ticks '(1 1.5)
                             #:ensure-max-tick? #f
                             #:ensure-min-tick? #f)))
  (render (with (graph (row-df [major minor money]
                               "expenses" "food" 20
                               "expenses" "transport" 30
                               "expenses" "laundry" 10
                               "expenses" "laundry" 5
                               "income" "paycheck" 100
                               "income" "side-job" 10))
                (make-stacked-bars #:category "major"
                                   #:subcategory "minor"
                                   #:value "money"
                                   #:labels? #f)
                (make-x-axis)
                (make-y-axis #:min 0)))
  (render (with (graph (row-df [major minor money]
                               "expenses" "food" 20
                               "expenses" "transport" 30
                               "expenses" "laundry" 10
                               "expenses" "laundry" 5
                               "income" "paycheck" 100
                               "income" "side-job" 10))
                (make-stacked-bars #:category "major"
                                   #:subcategory "minor"
                                   #:value "money")
                (make-x-axis)
                (make-y-axis #:min 0)))

  (render (with (graph (row-df [a] 5))
                (make-function (λ (x) (expt 2 x))
                               #:min 1 #:max 100)
                (make-x-axis #:min 1 #:max 100)
                (make-y-axis #:layout 'log)))

  (require rackunit)
  (check-equal? (renderer->plot:data (row-df [major minor money]
                                             "expenses" "food" 20
                                             "expenses" "transport" 30
                                             "expenses" "laundry" 10
                                             "expenses" "laundry" 5
                                             "income" "paycheck" 100
                                             "income" "side-job" 10)
                                     (make-stacked-bars #:category "major"
                                                        #:subcategory "minor"
                                                        #:value "money"))
                ;; 15 before 30 because of sorting
                '(("expenses" (20 15 30))
                  ("income" (100 10))))
  (check-equal? (let ([data (row-df [major minor money]
                                    "expenses" "food" 20
                                    "expenses" "transport" 30
                                    "expenses" "laundry" 10
                                    "expenses" "laundry" 5
                                    "income" "paycheck" 100
                                    "income" "side-job" 10)])
                  (data->stacked-bar-label-info
                   data
                   (renderer->plot:data
                    data
                    (make-stacked-bars #:category "major"
                                       #:subcategory "minor"
                                       #:value "money"
                                       #:labels? #t))
                   "major"
                   "minor"))
                '((0.5 50 "transport")
                  (0.5 55/2 "laundry")
                  (0.5 10 "food")
                  (1.5 105 "side-job")
                  (1.5 50 "paycheck"))))
