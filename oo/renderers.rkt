#lang at-exp racket

(require (prefix-in plot: plot)
         data-frame
         sawzall
         math/statistics
         syntax/parse/define
         "../error-reporting.rkt"
         "../util.rkt")

(define plot-renderer-tree/c any/c)
(struct point+label (point label))

(define renderer<%>
  (interface ()
    ;; precondition: the data-frame must have been transformed with `converter-transform`
    [->plot-data (->m data-frame? any/c)]
    ;; precondition: the data-frame must have been transformed with `converter-transform`
    [->plot-renderer-tree (->m data-frame? plot-renderer-tree/c)]
    [rightmost-points+labels (->m data-frame? (listof point+label?))]
    [base-axis (->m (or/c 'x 'y))]
    [creates-own-base-axis? (->m boolean?)]
    [has-categorical-base-axis? (->m boolean?)]))

(define current-complot-printer
  (make-parameter (λ (thing port)
                    ;; This @~a{...} syntax is like string interpolation in other languages
                    ;; It creates a string that can embed code and values using @
                    (display @~a{#<complot @(object-name thing)>}
                             port))))

(define complot-printable%
  (class* object% (writable<%>)
    (super-new)
    (define/public (custom-write out)
      (apply (current-complot-printer) this out))
    (define/public (custom-display out)
      (custom-write out))))

(define axis%
  (class complot-printable%
    (super-new)
    (init-field label
                ticks?
                major-ticks-every
                minor-ticks-between-major
                tick-lines?
                min
                max
                layout
                ensure-min-tick?
                ensure-max-tick?
                minimum-ticks
                x-or-y)))
(define x-axis%
  (class axis%
    (super-new [x-or-y 'x])))
(define y-axis%
  (class axis%
    (super-new [x-or-y 'y])))

(define legend%
  (class object%
    (super-new)
    (init-field position
                type)))
(define title%
  (class complot-printable%
    (super-new)
    (init-field text)))


(define-simple-macro (define-simple-renderer n-dim:number (id:id formals ...)
                       (make-it ...))
  #:with [[[y-conv-def ...] [y-conv ...]] ...] (if (= (syntax->datum #'n-dim) 2)
                                       #'[[[#:y-converter [y-converter #f]] [[y-converter y-converter]]]]
                                       #'[])
  (define (id formals ...
              #:color [color 'auto]
              #:alpha [alpha 1]
              #:size [size 'auto]
              #:type [type 'auto]
              #:label [label 'auto]

              #:x-converter [x-converter #f]
              y-conv-def ... ...)
    (make-it ...
         [color color]
         [alpha alpha]
         [size size]
         [type type]
         [label label]
         [x-converter x-converter]
         y-conv ... ...)))
(define-simple-renderer 2 (make-point-label x y content
                                            #:anchor [anchor 'auto])
  (new point-label% [x x] [y y] [content content] [anchor anchor]))
(define-simple-renderer 2 (make-points #:x x
                                       #:y y)
  (new points% [x-col x] [y-col y]))
(define-simple-renderer 2 (make-line #:x x
                                     #:y y)
  (new line% [x-col x] [y-col y]))
(define-simple-renderer 2 (make-bars #:x x
                                     #:y y
                                     #:invert? [invert? #f])
  (new bars% [x-col x] [y-col y] [invert? invert?]))
(define-simple-renderer 1 (make-histogram #:x x
                                          #:invert? [invert? #f]
                                          #:bins [bins 30])
  (new histogram% [x-col x] [invert? invert?] [bins bins]))

(define-simple-macro (define-simple-3v-renderer (id:id formals ...)
                       (make-it ...))
  (define (id formals ...
              #:colors [colors 'auto]
              #:alphas [alphas 1]
              #:size [size 'auto]
              #:type [type 'auto]
              #:labels [labels 'auto]

              #:aggregate [aggregator 'auto]

              #:x-converter [x-converter #f]
              #:y-converter [y-converter #f]
              #:group-converter [group-converter #f])
    (make-it ...
         [colors colors]
         [alphas alphas]
         [size size]
         [type type]
         [labels labels]
         [aggregator aggregator]
         [x-converter x-converter]
         [y-converter y-converter]
         [group-converter group-converter])))

(define-simple-3v-renderer (make-stacked-bars #:x x-col
                                              #:group-by group-col
                                              #:y y-col
                                              #:invert? [invert? #f]
                                              #:auto-label? [auto-label? #t])
  (new stacked-bars%
       [x-col x-col]
                 [y-col y-col]
                 [group-col group-col]
                 [invert? invert?]
                 [labels? auto-label?]))

(define-simple-3v-renderer (make-stacked-area #:x x-col
                                              #:group-by group-col
                                              #:y y-col
                                              #:auto-label? [auto-label? #t])
  (new stacked-area%
       [x-col x-col]
                 [y-col y-col]
                 [group-col group-col]
                 [labels? auto-label?]))
                                              
(define-simple-renderer 1 (make-function f
                                         #:min min
                                         #:max max)
  (new function% [f f] [min min] [max max]))


(define single-mark-renderer%
  (class complot-printable%
    (super-new)
    (init-field color
                alpha
                size
                type
                label)
    (define/public (base-axis) 'x)
    (define/public (creates-own-base-axis?) #f)
    (define/public (has-categorical-base-axis?) #f)))

(define renderer-1v%
  (class single-mark-renderer%
    (super-new)
    (init-field x-col
                x-converter)
    (define/public (get-label)
      (if-auto (get-field label this)
               (get-field x-col this)))))

(define renderer-2v%
  (class renderer-1v%
    (super-new)
    (init-field y-col
                y-converter)
    (define/override (get-label)
      (if-auto (get-field label this)
               (get-field y-col this)))))

(define renderer-multi-v%
  (class complot-printable%
    (super-new)
    (init-field colors
                alphas
                size
                type
                labels
                x-converter
                y-converter
                group-converter

                x-col
                y-col
                group-col
                group-ordering
                [group-aggregator +])
    (define/public (base-axis) 'x)
    (define/public (creates-own-base-axis?) #f)
    (define/public (has-categorical-base-axis?) #f)))


(define point-label%
  (class* single-mark-renderer% (renderer<%>)
    (super-new)
    (init-field x y content anchor angle)
    (define/public (->plot-data df) (list (list x y)))
    (define/public (->plot-renderer-tree df)
      (plot:point-label (list x y) content
                        #:anchor (or anchor (plot:label-anchor))
                        #:angle (or angle (plot:label-angle))))
    (define/public (rightmost-points+labels df) empty)))

(define (mixin:->plot-data-direct-x-and-y c)
  (class c
    (super-new)
    (inherit-field x-col y-col)
    (define/public (->plot-data data)
      (for/list ([x-y-vec (in-vector (df-select* data
                                                 x-col
                                                 y-col))])
        (vector->list x-y-vec)))))

(define (mixin:rightmost-points+labels-direct-x-and-y c)
  (class c
    (super-new)
    (inherit ->plot-data
             get-label)
    (define/public (rightmost-points+labels data)
      (define raw-data (->plot-data data))
      (define sorted-data (sort raw-data < #:key first))
      (list (point+label (last sorted-data)
                         (get-label))))))

#;(define (mixin:->plot-renderer-tree-simple c
                                           plot-renderer
                                           get-default-color
                                           get-default-alpha
                                           get-default-size
                                           get-default-))

;; lltodo: should have mixin that adds a get-color method which does this if-auto logic. You just pass in what the default color should be.
(define points%
  (class (mixin:rightmost-points+labels-direct-x-and-y
          (mixin:->plot-data-direct-x-and-y
           renderer-2v%))
    (super-new)
    (define/public (->plot-renderer-tree data
                                         #:bar-x-ticks? bar-x-ticks?
                                         #:bar-y-ticks? bar-y-ticks?
                                         #:legend? add-legend?)
      (plot:points (send this ->plot-data data)
                   #:color (if-auto (get-field color this) (plot:point-color))
                   #:alpha (if-auto (get-field alpha this) (plot:point-alpha))
                   #:size (if-auto (get-field size this) (plot:point-size))
                   #:sym (if-auto (get-field type this) (plot:point-sym))
                   #:label (and add-legend? (send this get-label))))))

(define line%
  (class (mixin:rightmost-points+labels-direct-x-and-y
          (mixin:->plot-data-direct-x-and-y
           renderer-2v%))
    (super-new)
    (define/public (->plot-renderer-tree data
                                         #:bar-x-ticks? bar-x-ticks?
                                         #:bar-y-ticks? bar-y-ticks?
                                         #:legend? add-legend?)
      (plot:lines (send this ->plot-data data)
                  #:color (if-auto (get-field color this) (plot:line-color))
                  #:alpha (if-auto (get-field alpha this) (plot:line-alpha))
                  #:width (if-auto (get-field size this) (plot:line-width))
                  #:style (if-auto (get-field type this) (plot:line-style))
                  #:label (and add-legend? (send this get-label))))))

(define points+line%
  (class points%
    (super-new)
    (define/override (->plot-renderer-tree data
                                           #:bar-x-ticks? bar-x-ticks?
                                           #:bar-y-ticks? bar-y-ticks?
                                           #:legend? add-legend?)
      (define points-renderer (super ->plot-renderer-tree
                                     data
                                     #:bar-x-ticks? bar-x-ticks?
                                     #:bar-y-ticks? bar-y-ticks?
                                     #:legend? add-legend?))
      (define lines-renderer
        (plot:lines (send this ->plot-data data)
                    #:color (if-auto (get-field color this) (plot:line-color))
                    #:alpha (if-auto (get-field alpha this) (plot:line-alpha))
                    #:width (if-auto (get-field size this) (plot:line-width))
                    #:style (if-auto (get-field type this) (plot:line-style))
                    ;; label explicitly omitted, to not get double labeling of this
                    ))
      (list points-renderer
            lines-renderer))))

(define (mixin:invertable c)
  (class c
    (super-new)
    (init-field invert?)
    (define/override (base-axis)
      (if invert? 'y 'x))))

(define bars%
  (class ((compose1 mixin:rightmost-points+labels-direct-x-and-y
                    mixin:->plot-data-direct-x-and-y
                    mixin:invertable)
          renderer-2v%)
    (super-new)
    (define/public (->plot-renderer-tree data
                                         #:bar-x-ticks? bar-x-ticks?
                                         #:bar-y-ticks? bar-y-ticks?
                                         #:legend? add-legend?)
      (define is-categorical-plot?
        (categorical? data
                      (dynamic-get-field (string->symbol (~a (send this base-axis) "-col"))
                                         this)))
      (if is-categorical-plot?
          (plot:discrete-histogram
           (send this ->plot-data data)
           #:color (if-auto (get-field color this) (plot:rectangle-color))
           #:alpha (if-auto (get-field alpha this) (plot:rectangle-alpha))
           #:line-width (if-auto (get-field size this) (plot:rectangle-line-width))
           #:style (if-auto (get-field type this) (plot:rectangle-style))
           #:invert? (get-field invert? this)
           #:add-ticks? (match (send this base-axis)
                          ['x bar-x-ticks?]
                          ['y bar-y-ticks?])
           #:label (and add-legend? (send this get-label)))
          (plot:rectangles
           (bars-data->rectangles (send this ->plot-data data)
                                  (get-field invert? this))
           #:color (if-auto (get-field color this) (plot:rectangle-color))
           #:alpha (if-auto (get-field alpha this) (plot:rectangle-alpha))
           #:line-width (if-auto (get-field size this) (plot:rectangle-line-width))
           #:style (if-auto (get-field type this) (plot:rectangle-style))
           #:label (and add-legend? (send this get-label)))))

    (define/private (bars-data->rectangles raw-bars-data)
      (define xs (map first raw-bars-data))
      (define x-range (- (apply max xs) (apply min xs)))
      (define bar-width/2 (/ (* (complot-rectangle-width)
                                (/ x-range (length xs)))
                             2))
      (define bar->rectangle
        (match-lambda
          [(list x y)
           (list (plot:ivl (- x bar-width/2) (+ x bar-width/2))
                 (plot:ivl 0 y))]))
      (define rectangles-data
        (map bar->rectangle raw-bars-data))
      (define (invert-rectangles data)
        (map (match-lambda [(list x y) (list y x)]) data))
      (if (get-field invert? this)
          (invert-rectangles rectangles-data)
          rectangles-data))))

(define complot-rectangle-width (make-parameter 0.8))


(define (mixin:groupable c)
  (class c
    (super-new)
    (inherit-field group-ordering
                   group-col)
    (define/public (get-group-ordering)
      (match group-ordering
        [(? procedure? f) f]
        [else orderable<?]))
    (define/public (get-groups data)
      (sort (remove-duplicates (vector->list (df-select data group-col)))
            (get-group-ordering)))))

(define (mixin:multicolored c)
  (class c
    (super-new)
    (inherit-field colors)
    (define/public (get-color-sequence)
      (match colors
        ['auto (in-naturals)]
        [(? list? l)
         (stream-append (list->stream l)
                        (stream-remove* (in-naturals) l))]
        [else (error 'get-color-sequence "bad colors field value")]))))

(define (stream-remove* s to-remove)
  (stream-filter s (λ (v) (not (member v to-remove)))))

(define (list->stream l)
  (for/stream ([v (in-list l)])
    v))

(define (sort-data-by-x-col-then-groups data x-col group-col group-ordering)
  (define sorted-by-x (reorder data x-col))
  (define grouped-by-x (group-with sorted-by-x x-col))
  (define sorted-within-x (reorder grouped-by-x (cons group-col group-ordering)))
  (ungroup sorted-within-x)

  ;; (define cols (df-series-names sorted-data))
  ;; (define group-sequences
  ;;   (df-select
  ;;    (aggregate (group-with (rename sorted-data
  ;;                                  group-col
  ;;                                  "GROUP")
  ;;                          x-col)
  ;;               [GROUP (GROUP) (vector->list GROUP)])
  ;;    "GROUP"))
  ;; (define group-sequence (remove-duplicates (append* (vector->list group-sequences))))
  )

(define (find-group-df df-partitions-by-group-col group-col group)
  (findf (λ (df) (equal? group (any-value-in df group-col)))
         df-partitions-by-group-col))
(define (any-value-in data col)
  (vector-ref (df-select data col) 0))

(define stacked-bars%
  (class (mixin:multicolored (mixin:groupable (mixin:invertable renderer-multi-v%)))
    (super-new)
    (init-field [auto-label? #f])
    (inherit get-groups
             get-group-ordering)
    (inherit-field x-col
                   y-col
                   group-col
                   group-aggregator)
    (define/public (->plot-data data)
      #|
      | group    | category  | money |
      |----------+-----------+-------|
      | expenses | food      |    20 |
      |          | transport |    30 |
      |          | laundry   |    10 |
      | income   | paycheck  |   100 |
      |          | side-job  |    10 |
      ^          ^           ^
      x-col      group-col    y-col
      |#
      (define sorted-data
        (sort-data-by-x-col-then-groups data
                                        x-col
                                        group-col
                                        (get-group-ordering)))
      (define group-sequence (get-groups data))
      (for/list ([x-col-group-df (in-list (split-with sorted-data x-col))])
        (define x-value (any-value-in x-col-group-df x-col))
        (define group-col-dfs (split-with x-col-group-df group-col))
        (list x-value
              (for/list ([group (in-list group-sequence)])
                (define group-df (find-group-df group-col-dfs
                                                group-col
                                                group))
                (if group-df
                    (apply group-aggregator (vector->list (df-select group-df y-col)))
                    0)))))
    (define/public (->plot-renderer-tree data
                                         #:bar-x-ticks? bar-x-ticks?
                                         #:bar-y-ticks? bar-y-ticks?
                                         #:legend? add-legend?)
      (define raw-data (send this ->plot-data data))
      (list
       (plot:stacked-histogram raw-data
                               #:color (if-auto (get-field colors this)
                                                (plot:stacked-histogram-colors))
                               #:alpha (if-auto (get-field alphas this)
                                                (plot:stacked-histogram-alphas))
                               #:line-widths (if-auto (get-field size this)
                                                      (plot:stacked-histogram-line-widths))
                               #:styles (if-auto (get-field type this)
                                                 (plot:stacked-histogram-styles))
                               #:invert? (get-field invert? this)
                               #:add-ticks? (match (send this base-axis)
                                              ['x bar-x-ticks?]
                                              ['y bar-y-ticks?])
                               #:label (if add-legend?
                                           (if-auto (get-field labels this)
                                                    (map ~a (get-groups data)))
                                           '(#f)))
       (if auto-label?
           (make-stacked-bar-labels (data->stacked-bar-label-info data
                                                                  raw-data))
           empty)))

    (define/private (make-stacked-bar-labels label-info ;; as returned by `data->stacked-bar-label-info`
                                             )
      (for/list ([info (in-list label-info)])
        (match-define (list x-pos y-pos label) info)
        (plot:point-label (if (get-field invert? this)
                              (list y-pos x-pos)
                              (list x-pos y-pos))
                          (~a label)
                          #:anchor 'center
                          #:point-size 0)))

    (define/private (data->stacked-bar-label-info data
                                                  raw-data)
      (append*
       (for/list ([x-col-group    (in-list raw-data)]
                  [x-pos          (in-range 0.5 (+ 0.5 (length raw-data)))])
         (for/fold ([points empty]
                    [bar-height-so-far 0]
                    #:result points)
                   ([group-value (in-list (get-groups data))]
                    [y-value     (in-list (second x-col-group))]
                    #:when (> y-value 0))
           (values (cons (list x-pos
                               (+ bar-height-so-far (/ y-value 2))
                               group-value)
                         points)
                   (+ bar-height-so-far y-value))))))))

(define (categorical? data col)
  (match (df-select data col)
    [(vector (? real?) ...) #f]
    [else #t]))

(define histogram%
   (class (mixin:invertable renderer-1v%)
     (super-new)
     (init-field [bins 30])
     (inherit-field invert?
                    x-col)
     (define/public (->plot-data data)
       (define the-values (vector->list (df-select data x-col)))
       (cond [(or (categorical? data x-col)
                  (< (length (remove-duplicates the-values)) bins))
              (define frequencies
                (samples->hash the-values))
              (define histogram-data (hash-map frequencies list))
              (if (categorical? data x-col)
                  histogram-data
                  (sort histogram-data < #:key first))]
             [else
              ;; todo: improvement, this should be able to bin based on the axis bounds?
              ;; or have a seperate argument to say the bounds of binning.
              (define values-min (apply min the-values))
              (define values-max (apply max the-values))
              (define bin-bounds (range values-min
                                        values-max
                                        (/ (- values-max values-min) bins)))
              (for/list ([a-bin (bin-samples bin-bounds <= the-values)])
                (list (sample-bin-min a-bin)
                      (length (sample-bin-values a-bin))))]))

     (define/public (->plot-renderer-tree data
                                          #:bar-x-ticks? bar-x-ticks?
                                          #:bar-y-ticks? bar-y-ticks?
                                          #:legend? add-legend?)
       (plot:discrete-histogram (send this ->plot-data data)
                                #:color (if-auto (get-field color this)
                                                 (plot:rectangle-color))
                                #:alpha (if-auto (get-field alpha this)
                                                 (plot:rectangle-alpha))
                                #:line-width (if-auto (get-field size this)
                                                      (plot:rectangle-line-width))
                                #:style (if-auto (get-field type this)
                                                 (plot:rectangle-style))
                                #:invert? invert?
                                #:add-ticks? (match (send this base-axis)
                                               ['x bar-x-ticks?]
                                               ['y bar-y-ticks?])
                                #:label (and add-legend?
                                             (or (get-field label this)
                                                 (~a x-col " count")))))))


;; (listof points-list?) -> (listof points-list?)
;; Assumes that all of the points in grouped-points are aligned. See `check-aligned!`.
(define (grouped-points->stacked-points grouped-points)
  (for/fold ([last-points #f]
             [stacked-points empty]
             #:result (reverse stacked-points))
            ([points (in-list grouped-points)])
    (define stacked
      (if last-points
          (map (match-lambda** [{(list x y) (list _ y-base)} (list x (+ y y-base))])
               points
               last-points)
          points))
    (values stacked
            (cons stacked stacked-points))))

(define stacked-area%
  (class (mixin:multicolored (mixin:groupable renderer-multi-v%))
    (super-new)
    (inherit get-groups
             get-group-ordering)
    (inherit-field group-aggregator
                   x-col
                   y-col
                   group-col
                   labels
                   alphas)
    (define/public (->plot-data data)
      (define group-sequence (get-groups data))
      (define grouped-points
        (data->grouped-points data x-col y-col group-col group-sequence group-aggregator))
      (check-aligned! grouped-points
                      this
                      group-sequence)
      (grouped-points->stacked-points grouped-points))

    (define/public (->plot-renderer-tree data
                                         #:bar-x-ticks? bar-x-ticks?
                                         #:bar-y-ticks? bar-y-ticks?
                                         #:legend? add-legend?)
      (define ->0 (match-lambda [(list x y) (list x 0)]))
      (define groups (get-groups data))
      (for/fold ([plot:renderers empty]
                 [last-group-points #f]
                 #:result plot:renderers)
                ([group (in-list groups)]
                 [group-points (in-list (send this ->plot-data data))]
                 [color (send this get-color-sequence)]
                 [alpha (if-auto alphas (in-cycle (in-value (plot:interval-alpha))))]
                 [group-label (in-list (if-auto labels groups))])
        (define this-renderer
          (plot:lines-interval (or last-group-points
                                   (list (->0 (argmin first group-points))
                                         (->0 (argmax first group-points))))
                               group-points
                               #:color color
                               #:line1-color color
                               #:line2-width 0
                               #:alpha alpha
                               #:label (and add-legend? (~a group-label))))
        (values (cons this-renderer plot:renderers)
                group-points)))

    ;; data-frame? string? string? string?
    ;; ->
    ;; (listof points-list?)
    ;; where points-list? := (listof (list/c real? real?))
    ;; Each points-list in the result is sorted by first / x values.
    (define/private (data->grouped-points data ;; assume that any converters have been applied already
                                          x-col
                                          y-col
                                          group-col
                                          groups
                                          aggregator)
      (define group-datas (split-with data group-col))
      (for*/list ([group (in-list groups)]
                  [group-data (in-value (find-group-df group-datas group-col group))])
        (define plain-points (send (make-points #:x x-col
                                                #:y y-col)
                                   ->plot-data
                                   group-data))
        (define sorted (sort plain-points < #:key first))
        (define aggregated
          (if (or (empty? sorted)
                  (false? aggregator))
              sorted
              (aggregate-by-x-values sorted aggregator)))
        aggregated))

    ;; non-empty-list? (any/c ... -> any/c) -> non-empty-list?
    (define/private (aggregate-by-x-values points aggregator)
      (define (aggregate x ys aggregated)
        (cons (list x (apply aggregator ys))
              aggregated))
      (for/fold ([current-x (first (first points))]
                 [current-ys empty]
                 [aggregated empty]
                 #:result (reverse (if (empty? current-ys)
                                       aggregated
                                       (aggregate current-x current-ys aggregated))))
                ([point (in-list points)])
        (match-define (list x y) point)
        (match x
          [(== current-x) (values current-x
                                  (cons y current-ys)
                                  aggregated)]
          [new-x (values new-x
                         (list y)
                         (aggregate current-x current-ys aggregated))])))))

(define function%
  (class single-mark-renderer%
    (super-new)
    (init-field f ; real? -> real?
                min ; real?
                max ; real?
                )
    (inherit-field color
                   alpha
                   size
                   type
                   label)
    (define/public (->plot-data data)
      (for/list ([x (in-range min max (/ (- max min) (plot:line-samples)))])
        (list x (f x))))
    (define/public (->plot-renderer-tree data
                                         #:bar-x-ticks? bar-x-ticks?
                                         #:bar-y-ticks? bar-y-ticks?
                                         #:legend? add-legend?)
      (plot:function f
                     min
                     max
                     #:color (if-auto color (plot:line-color))
                     #:alpha (if-auto alpha (plot:line-alpha))
                     #:width (if-auto size (plot:line-width))
                     #:style (if-auto type (plot:line-style))
                     #:label (and add-legend? label)))))

;; --- The plot struct ---
#;(struct plot complot-printable (data x-axis y-axis legend title renderers))

;; Some convenience macros
#;(define-simple-macro (plot-set a-plot field v)e
  (struct-copy plot a-plot [field v]))
#;(define-simple-macro (plot-update a-plot field f v)
  #:with get-field (format-id this-syntax "plot-~a" #'field)
  (struct-copy plot a-plot [field (f v (get-field a-plot))]))

#;(define (make-plot data)
  (plot data #f #f #f #f empty))


(define plot%
  (class complot-printable%
    (super-new)
    (init-field data
                [x-axis #f]
                [y-axis #f]
                [legend #f]
                [title #f]
                [renderers empty])))
(define (make-plot data)
  (new plot% [data data]))

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
    (new axis%
         [label label]
         [ticks? ticks?]
         [major-tick-every major-tick-every]
         [minor-ticks-between-major minor-ticks-between-major]
         [tick-lines? tick-lines?]
         [min min]
         [max max]
         [layout layout]
         [ensure-min-tick? ensure-min-tick?]
         [ensure-max-tick? ensure-max-tick?]
         [minimum-ticks minimum-ticks])))
(define-axis-maker make-x-axis x-axis%)
(define-axis-maker make-y-axis y-axis%)

(define (make-legend #:position [position 'auto]
                     #:type [type 'new])
  (new legend%
       [position position]
       [type type]))

