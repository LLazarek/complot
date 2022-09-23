#lang at-exp racket

(require (prefix-in plot: plot)
         (prefix-in plot: plot/utils)
         data-frame
         sawzall
         math/statistics
         syntax/parse/define
         "basics.rkt"
         "error-reporting.rkt"
         "util.rkt")

(provide renderer<%>

         point-label%
         points%
         line%
         points+line%
         bars%
         histogram%
         function%
         stacked-bars%
         stacked-area%
         ;; legend%
         ;; title%

         make-point-label
         make-points
         make-line
         make-points+line
         make-points
         make-bars
         make-histogram
         make-function
         ;; make-legend
         make-stacked-bars
         make-stacked-area

         (struct-out label-info))

(define plot-renderer-tree/c any/c)
(struct label-info (point label color))

(define renderer<%>
  (interface ()
    ;; precondition: the data-frame must have been transformed with `converter-transform`
    [->plot-data (->m data-frame? any/c)]
    ;; precondition: the data-frame must have been transformed with `converter-transform`
    [->plot-renderer-tree (->m data-frame?
                               #:bar-x-ticks? boolean?
                               #:bar-y-ticks? boolean?
                               #:legend? boolean?
                               plot-renderer-tree/c)]
    [new-style-legend-labels (->m data-frame? (listof label-info?))]
    [base-axis (->m (or/c 'x 'y))]
    [creates-own-base-axis? (->m boolean?)]
    [has-categorical-base-axis? (->m boolean?)]
    ;; for now, all that matters is 'bar and 'area
    [mark-type (->m (or/c 'bar 'point 'line 'area 'other))]
    [legend-compatibility (->m (or/c 'new 'old))]
    [legend-default (->m (or/c 'new 'old))]
    [get-color/s (->m (or/c plot:plot-color/c (listof plot:plot-color/c)))]))

;; (define legend%
;;   (class object%
;;     (super-new)
;;     (init-field position
;;                 type)))
;; (define title%
;;   (class complot-printable%
;;     (super-new)
;;     (init-field text)))


(define-simple-macro (define-simple-renderer n-dim:number (id:id formals ...)
                       (make-it ...))
  #:with [[[x-conv-def ...] [x-conv ...]] ...] (if (>= (syntax->datum #'n-dim) 1)
                                                   #'[[[#:x-converter [x-converter #f]] [[x-converter x-converter]]]]
                                                   #'[])
  #:with [[[y-conv-def ...] [y-conv ...]] ...] (if (= (syntax->datum #'n-dim) 2)
                                                   #'[[[#:y-converter [y-converter #f]] [[y-converter y-converter]]]]
                                                   #'[])
  (define (id formals ...
              #:color [color 'auto]
              #:alpha [alpha 1]
              #:size [size 'auto]
              #:type [type 'auto]
              #:label [label 'auto]

              x-conv-def ... ...
              y-conv-def ... ...)
    (make-it ...
             [color color]
             [alpha alpha]
             [size size]
             [type type]
             [label label]
             x-conv ... ...
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
(define-simple-renderer 2 (make-points+line #:x x
                                            #:y y)
  (new points+line% [x-col x] [y-col y]))
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
              #:alphas [alphas 'auto]
              #:size [size 'auto]
              #:type [type 'auto]
              #:labels [labels 'auto]

              #:aggregate [aggregator +]

              #:x-converter [x-converter #f]
              #:y-converter [y-converter #f]
              #:group-converter [group-converter #f])
    (make-it ...
             [colors colors]
             [alphas alphas]
             [size size]
             [type type]
             [labels labels]
             [group-aggregator aggregator]
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
       [auto-label? auto-label?]))

(define-simple-3v-renderer (make-stacked-area #:x x-col
                                              #:group-by group-col
                                              #:y y-col
                                              #:auto-label? [auto-label? #t])
  (new stacked-area%
       [x-col x-col]
       [y-col y-col]
       [group-col group-col]
       [auto-label? auto-label?]))

(define-simple-renderer 0 (make-function f
                                         #:min min
                                         #:max max)
  (new function% [f f] [min min] [max max]))

(define (mixin:->plot-data/protected c)
  (class c
    (super-new)
    (abstract ->plot-data/unchecked
              check-plot-data-type!
              convert-data)
    (define/public (->plot-data data)
      (define raw-data (send this ->plot-data/unchecked (send this convert-data data)))
      (send this check-plot-data-type! raw-data)
      raw-data)))

(define pre-single-mark-renderer%
  (class complot-printable%
    (super-new)
    (init-field color
                alpha
                size
                type
                label)
    (define/public (base-axis) 'x)
    (define/public (creates-own-base-axis?) #f)
    (define/public (has-categorical-base-axis?) #f)
    (define/public (mark-type) 'other)))
(define single-mark-renderer%
  (mixin:->plot-data/protected pre-single-mark-renderer%))

(define (mixin:convert-data c get-cols get-converters)
  (class c
    (super-new)
    (define data-cache (list #f #f))
    (define/override (convert-data df)
      (cond [(eq? df (first data-cache))
             (second data-cache)]
            [else
             (define converted
               (convert df
                        (get-cols this)
                        (get-converters this)))
             (set! data-cache (list df converted))
             converted]))))

(define (convert a-df cols converters)
  (df-transform-series a-df
                       (for/list ([col (in-list cols)]
                                  [f   (in-list converters)]
                                  ;; skip doing work if there's no converter
                                  #:when f)
                         (list col f))))

(define (df-transform-series a-df names-and-fns)
  (for/fold ([df a-df])
            ([name+fn (in-list names-and-fns)])
    (define name (first name+fn))
    (define fn (second name+fn))
    (define copy (df-shallow-copy df))
    (df-add-derived! copy
                     name
                     (list name)
                     (λ (value-in-list) (apply fn value-in-list)))
    copy))

(define-simple-macro (fields-getter field:id ...)
  (λ (o) (list (get-field field o) ...)))

(define renderer-1v%*
  (class single-mark-renderer%
    (super-new)
    (init-field x-col
                x-converter)
    (define/public (get-label)
      (if-auto (get-field label this)
               (get-field x-col this)))))
(define renderer-1v%
  (mixin:convert-data renderer-1v%*
                      (fields-getter x-col)
                      (fields-getter x-converter)))

(define renderer-2v%*
  (class renderer-1v%*
    (super-new)
    (init-field y-col
                y-converter)
    (inherit-field x-col
                   x-converter)
    (define/override (get-label)
      (if-auto (get-field label this)
               (get-field y-col this)))))
(define renderer-2v%
  (mixin:convert-data renderer-2v%*
                      (fields-getter x-col y-col)
                      (fields-getter x-converter y-converter)))

;; multi-v has different (pluralized) names for some fields, so it has to start
;; from scratch (in terms of the inheritance chain leveraged for 1v and 2v)
(define pre-renderer-multi-v%*
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
                [group-ordering 'auto]
                [group-aggregator +])
    (define/public (base-axis) 'x)
    (define/public (creates-own-base-axis?) #f)
    (define/public (has-categorical-base-axis?) #f)
    (define/public (mark-type) 'other)))
(define renderer-multi-v%*
  (mixin:->plot-data/protected pre-renderer-multi-v%*))
(define renderer-multi-v%
  (mixin:convert-data renderer-multi-v%*
                      (fields-getter x-col y-col group-col)
                      (fields-getter x-converter y-converter group-converter)))


(define ((make-color-mixin get-the-color-field) default)
  (λ (c)
    (class c
      (super-new)
      (define/public (get-color/s)
        (if-auto (get-the-color-field this) default)))))
(define mixin:color/default
  (make-color-mixin (λ (o) (get-field color o))))
(define mixin:colors/default
  (make-color-mixin (λ (o) (get-field colors o))))

(define point-label%
  (class* ((mixin:color/default (plot:point-color))
           single-mark-renderer%)
    (renderer<%>)
    (super-new)
    (init-field x y content anchor angle)
    (define/override (->plot-data/unchecked df) (list (list x y)))
    (define/override (check-plot-data-type! plot-data)
      (match plot-data
        [(list (? real?) (? real?)) (void)]
        [(list x y)
         (error 'complot
                @~a{
                    Error: point-labels can only take real numbers for their x and y position
                    but you made a point-label with something else:
                    x = @~s[x]
                    y = @~s[y]
                    })]))

    (define/public (->plot-renderer-tree df)
      (plot:point-label (list x y) content
                        #:anchor (or anchor (plot:label-anchor))
                        #:angle (or angle (plot:label-angle))
                        #:point-color (send this get-color/s)))
    (define/public (new-style-legend-labels df) empty)
    (define/public (legend-compatibility) 'new)
    (define/public (legend-default) 'new)))

(define (mixin:->plot-data-direct-x-and-y c)
  (class c
    (super-new)
    (inherit-field x-col y-col)
    (define/override (->plot-data/unchecked data)
      (for/list ([x-y-vec (in-vector (df-select* data
                                                 x-col
                                                 y-col))])
        (vector->list x-y-vec)))))

(define (mixin:new-style-legend-labels-direct-x-and-y c)
  (class c
    (super-new)
    (inherit ->plot-data
             get-label
             get-color/s)
    (define/public (new-style-legend-labels data)
      (define raw-data (->plot-data data))
      (define sorted-data (sort raw-data < #:key first))
      (list (label-info (last sorted-data)
                        (get-label)
                        (send this get-color/s))))))

(define ((mixin:real-real-plot-data name) c)
  (class c
    (super-new)
    (inherit-field x-col
                   y-col
                   x-converter
                   y-converter)
    (define/override (check-plot-data-type! plot-data)
      (match plot-data
        [(list (list (? real?) (? real?)) ...) (void)]
        [(list-no-order (and (not (list (? real?) (? real?)))
                             bad-point)
                        _ ...)
         (error 'complot
                @~a{
                    Error: @name can only use real number data, but I got a bit of data
                    for the @name with #:x @~s[x-col] and #:y @~s[y-col]
                    which is something else (x y):
                    @~s[bad-point]
                    There might be a problem in the data set, with the converters (shown below),
                    or you might have put a mistaken column for either #:x or #:y.
                    x-converter: @x-converter
                    y-converter: @y-converter
                    })]))))

#;(define (mixin:->plot-renderer-tree-simple c
                                             plot-renderer
                                             get-default-color
                                             get-default-alpha
                                             get-default-size
                                             get-default-))

(define points%
  (class* ((compose1 (mixin:real-real-plot-data 'points)
                     mixin:new-style-legend-labels-direct-x-and-y
                     mixin:->plot-data-direct-x-and-y
                     (mixin:color/default (plot:point-color)))
           renderer-2v%)
    (renderer<%>)

    (super-new)
    (define/public (->plot-renderer-tree data
                                         #:bar-x-ticks? bar-x-ticks?
                                         #:bar-y-ticks? bar-y-ticks?
                                         #:legend? add-legend?)
      (plot:points (send this ->plot-data data)
                   #:color (send this get-color/s)
                   #:alpha (if-auto (get-field alpha this) (plot:point-alpha))
                   #:size (if-auto (get-field size this) (plot:point-size))
                   #:sym (if-auto (get-field type this) (plot:point-sym))
                   #:label (and add-legend? (send this get-label))))
    (define/public (legend-compatibility) 'new)
    (define/public (legend-default) 'new)))

(define line%
  (class* ((compose1 (mixin:real-real-plot-data 'line)
                     mixin:new-style-legend-labels-direct-x-and-y
                     mixin:->plot-data-direct-x-and-y
                     (mixin:color/default (plot:line-color)))
           renderer-2v%)
    (renderer<%>)

    (super-new)
    (define/public (->plot-renderer-tree data
                                         #:bar-x-ticks? bar-x-ticks?
                                         #:bar-y-ticks? bar-y-ticks?
                                         #:legend? add-legend?)
      (plot:lines (send this ->plot-data data)
                  #:color (send this get-color/s)
                  #:alpha (if-auto (get-field alpha this) (plot:line-alpha))
                  #:width (if-auto (get-field size this) (plot:line-width))
                  #:style (if-auto (get-field type this) (plot:line-style))
                  #:label (and add-legend? (send this get-label))))
    (define/public (legend-compatibility) 'new)
    (define/public (legend-default) 'new)))

(define points+line%
  (class* points% (renderer<%>)
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
                    #:color (send this get-color/s)
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
(define (mixin:bar-plot c)
  (class c
    (super-new)
    (define/override (mark-type) 'bar)))

(define (mixin:new-style-legend-unsupported c)
  (class c
    (super-new)
    (define/public (legend-compatibility) 'old)
    (define/public (legend-default) 'old)
    (define/public (new-style-legend-labels df) empty)))

(define bars%
  (class* ((compose1 mixin:->plot-data-direct-x-and-y
                     mixin:invertable
                     mixin:bar-plot
                     mixin:new-style-legend-unsupported
                     (mixin:color/default (plot:rectangle-color)))
           renderer-2v%)
    (renderer<%>)

    (super-new)
    (inherit-field x-col
                   y-col
                   y-converter)
    (define/override (check-plot-data-type! plot-data)
      (match plot-data
        [(list (list _ (? real?)) ...) (void)]
        [(list-no-order (list _ (and bad-y-val (not (? real?))))
                        _ ...)
         (error 'complot
                @~a{
                    Error: bars can only use real number data for its #:y values, but I got a value
                    from the data for the bars with #:x @~s[x-col] and #:y @~s[y-col] for which
                    the y value is something else:
                    @~s[bad-y-val]
                    There might be a problem in the data set, with the converters (shown below),
                    or you might have put a mistaken column for #:y.
                    y-converter: @y-converter
                    })]))
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
           #:color (send this get-color/s)
           #:alpha (if-auto (get-field alpha this) (plot:rectangle-alpha))
           #:line-width (if-auto (get-field size this) (plot:rectangle-line-width))
           #:style (if-auto (get-field type this) (plot:rectangle-style))
           #:invert? (get-field invert? this)
           #:add-ticks? (match (send this base-axis)
                          ['x bar-x-ticks?]
                          ['y bar-y-ticks?])
           #:label (and add-legend? (send this get-label)))
          (plot:rectangles
           (bars-data->rectangles (send this ->plot-data data))
           #:color (send this get-color/s)
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
    (inherit get-color/s)
    (define/public (get-color-sequence n)
      (match (send this get-color/s)
        [(? procedure? f) (f n)] ;; see plot docs for `plot-colors/c`
        [(? list? l)
         #:when (>= (length l) n)
         (take l n)]
        [(? list? l)
         (stream-append (list->stream l)
                        (stream-remove* (in-naturals) l))]
        [other (error 'get-color-sequence "bad colors field value: ~e" other)]))))

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
  (class* ((compose1 mixin:multicolored
                     (mixin:colors/default (plot:stacked-histogram-colors))
                     mixin:groupable
                     mixin:invertable
                     mixin:bar-plot
                     mixin:new-style-legend-unsupported)
           renderer-multi-v%)
    (renderer<%>)

    (super-new)
    (init-field [auto-label? #f])
    (inherit get-groups
             get-group-ordering)
    (inherit-field x-col
                   y-col
                   group-col
                   group-aggregator
                   x-converter
                   y-converter
                   group-converter)
    (define/override (->plot-data/unchecked data)
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

    (define/override (check-plot-data-type! plot-data)
      (match plot-data
        [(list (list _ (list (? real?) ...)) ...) (void)]
        [(list-no-order (list x-value (and (not (list (? real?) ...)) bad-group-vals))
                        _ ...)
         (error 'complot
                @~a{
                    Error: stacked-bars can only use real number data for its #:y values, but I got
                    a value from the data for the stacked-bars with
                    #:x @~s[x-col] and #:y @~s[y-col] and #:group-by @~s[group-col] for which
                    the y values are something else:
                    x: @~s[x-value]
                    y-values: @~s[bad-group-vals]

                    The converters for this renderer are:
                    x-converter: @x-converter
                    y-converter: @y-converter
                    group-converter: @group-converter
                    })]))

    (define/public (->plot-renderer-tree data
                                         #:bar-x-ticks? bar-x-ticks?
                                         #:bar-y-ticks? bar-y-ticks?
                                         #:legend? add-legend?)
      (define raw-data (send this ->plot-data data))
      (list
       (plot:stacked-histogram raw-data
                               #:colors (if-auto (get-field colors this)
                                                 (plot:stacked-histogram-colors))
                               #:alphas (if-auto (get-field alphas this)
                                                 (plot:stacked-histogram-alphas))
                               #:line-widths (if-auto (get-field size this)
                                                      (plot:stacked-histogram-line-widths))
                               #:styles (if-auto (get-field type this)
                                                 (plot:stacked-histogram-styles))
                               #:invert? (get-field invert? this)
                               #:add-ticks? (match (send this base-axis)
                                              ['x bar-x-ticks?]
                                              ['y bar-y-ticks?])
                               #:labels (if add-legend?
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
  (class* ((compose1 (mixin:color/default (plot:rectangle-color))
                     mixin:invertable
                     mixin:bar-plot
                     mixin:new-style-legend-unsupported)
           renderer-1v%)
    (renderer<%>)

    (super-new)
    (init-field [bins 30])
    (inherit-field invert?
                   x-col
                   x-converter)
    (define/override (->plot-data/unchecked data)
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

    (define/override (check-plot-data-type! plot-data) (void))

    (define/public (->plot-renderer-tree data
                                         #:bar-x-ticks? bar-x-ticks?
                                         #:bar-y-ticks? bar-y-ticks?
                                         #:legend? add-legend?)
      (plot:discrete-histogram (send this ->plot-data data)
                               #:color (send this get-color/s)
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
  (class* ((compose1 mixin:multicolored
                     (mixin:colors/default (plot:stacked-histogram-colors))
                     mixin:groupable)
           renderer-multi-v%)
    (renderer<%>)

    (super-new)
    (init-field [auto-label? #f])
    (inherit get-groups
             get-group-ordering)
    (inherit-field group-aggregator
                   x-col
                   y-col
                   group-col
                   labels
                   alphas

                   x-converter
                   y-converter
                   group-converter)
    (define/override (->plot-data/unchecked data)
      (define group-sequence (get-groups data))
      (define grouped-points
        (data->grouped-points data x-col y-col group-col group-sequence group-aggregator))
      (check-aligned! grouped-points
                      this
                      group-sequence)
      (grouped-points->stacked-points grouped-points))

    (define/override (check-plot-data-type! plot-data)
      (match plot-data
        [(list (list (list (? real?) (? real?)) ...) ...) (void)]
        [(list-no-order (list-no-order (and (not (list (? real?) (? real?))) bad-point)
                                    _ ...)
                        _ ...)
         (error 'complot
            @~a{
                Error: stacked-area can only use real number data for its #:x and #:y values,
                but I got a value from the data for the stacked-area with
                #:x @~s[x-col] and #:y @~s[y-col] and #:group-by @~s[group-col] for which there
                is a point that is something else:
                @~s[bad-point]

                The converters for this renderer are:
                x-converter: @x-converter
                y-converter: @y-converter
                group-converter: @group-converter
                })]))

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
                         (aggregate current-x current-ys aggregated))])))

    (define/public (new-style-legend-labels data)
      (define raw-data (send this ->plot-data data))
      (define point-labels
        (cond [auto-label?
               (map ~a (get-groups data))]
              [(and (list? labels)
                    (= (length raw-data)
                       (length labels)))
               labels]
              [else
               (error 'complot
                      @~a{
                          Don't know how to label the stacked area plot.
                          Either enable automatic labeling with #:labels? #t
                          or provide a list of labels with #:label
                          which matches the number of distinct groups in
                          the data.
                          })]))
      (for/list ([points (in-list raw-data)]
                 [label  (in-list point-labels)]
                 [color  (in-list (send this get-color-sequence))])
        (label-info (last points) ; leverage the sorting of points that happens in ->plot-data
                    label
                    color)))

    (define/override (mark-type) 'area)
    (define/public (legend-compatibility) 'new)
    (define/public (legend-default) 'new)))

(define function%
  (class* ((compose1 (mixin:color/default (plot:line-color))
                     mixin:new-style-legend-unsupported)
           single-mark-renderer%)
    (renderer<%>)

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
    (define/override (->plot-data/unchecked data)
      (for/list ([x (in-range min max (/ (- max min) (plot:line-samples)))])
        (list x (f x))))
    (define/override (check-plot-data-type! plot-data) (void))
    (define/public (->plot-renderer-tree data
                                         #:bar-x-ticks? bar-x-ticks?
                                         #:bar-y-ticks? bar-y-ticks?
                                         #:legend? add-legend?)
      (plot:function f
                     min
                     max
                     #:color (send this get-color/s)
                     #:alpha (if-auto alpha (plot:line-alpha))
                     #:width (if-auto size (plot:line-width))
                     #:style (if-auto type (plot:line-style))
                     #:label (and add-legend? label)))
    (define/override (convert-data data) data)))

;; (define (make-legend #:position [position 'auto]
;;                      #:type [type 'new])
;;   (new legend%
;;        [position position]
;;        [type type]))

