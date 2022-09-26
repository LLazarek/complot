#lang at-exp racket

(provide renderers->plot:renderer-tree
         renderer->plot:renderer-tree
         renderer->plot:data
         bar-plot?
         (struct-out p+l)
         renderer->rightmost-points+labels)

(require "structs.rkt"
         "util.rkt"
         "error-reporting.rkt"
         (prefix-in plot: plot)
         data-frame
         math/statistics
         sawzall)

(define (renderer->plot:renderer-tree unconverted-data a-renderer
                                      #:bar-x-ticks? bar-x-ticks?
                                      #:bar-y-ticks? bar-y-ticks?
                                      #:legend? add-legend?)
  (define data (converter-transform unconverted-data a-renderer))
  (define raw-data (renderer->plot:data unconverted-data a-renderer data))
  (check-plot:data-types! a-renderer data raw-data)
  (match-define (renderer (appearance color alpha size type label) _)
    a-renderer)
  (define (color-sequence)
    (if (equal? color 'auto)
        (in-naturals)
        (in-sequences
         (if (list? color)
             color
             (list color))
         (in-naturals))))
  (match a-renderer
    [(struct* point-label ([content content]
                           [anchor anchor]))
     (plot:point-label raw-data
                       content
                       #:color (if-auto color (plot:plot-foreground))
                       #:alpha (if-auto alpha (plot:label-alpha))
                       #:point-size (if-auto size (plot:label-point-size))
                       #:point-sym (if-auto type 'fullcircle)
                       #:anchor anchor)]
    [(struct* points ([y-col y-col]
                      [group-col group-col]))
     (cond [group-col
            (define groups (group-ordering data group-col))
            (define group-datas (split-with data group-col))
            (for/list ([group (in-list groups)]
                       [group-color (color-sequence)]
                       #:when #t
                       [group-data (in-value (find-group-df group-datas group-col group))]
                       #:when (and group-data
                                   (> (df-row-count group-data) 0)))
              (define group-col-value (df-select group-data group-col))
              (plot:points (renderer->plot:data group-data a-renderer)
                           #:color (if-auto group-color (plot:point-color))
                           #:alpha (if-auto alpha (plot:point-alpha))
                           #:size (if-auto size (plot:point-size))
                           #:sym (if-auto type (plot:point-sym))
                           #:label (and add-legend? (match group-col-value
                                                      [(vector v _ ...) (~a v)]
                                                      [else #f]))))]
           [else
            (plot:points raw-data
                         #:color (if-auto color (plot:point-color))
                         #:alpha (if-auto alpha (plot:point-alpha))
                         #:size (if-auto size (plot:point-size))
                         #:sym (if-auto type (plot:point-sym))
                         #:label (and add-legend? (or (if-auto label y-col) y-col)))])]
    [(struct* line ([y-col y-col]))
     (plot:lines raw-data
                 #:color (if-auto color (plot:line-color))
                 #:alpha (if-auto alpha (plot:line-alpha))
                 #:width (if-auto size (plot:line-width))
                 #:style (if-auto type (plot:line-style))
                 #:label (and add-legend? (or (if-auto label y-col) y-col)))]
    [(struct* bars ([x-col x-col]
                    [y-col y-col]
                    [invert? invert?]))
     #:when (or (categorical? data x-col)
                (categorical? data y-col))
     (plot:discrete-histogram raw-data
                              #:color (if-auto color (plot:rectangle-color))
                              #:alpha (if-auto alpha (plot:rectangle-alpha))
                              #:line-width (if-auto size (plot:rectangle-line-width))
                              #:style (if-auto type (plot:rectangle-style))
                              #:invert? (or invert? (categorical? data y-col))
                              #:add-ticks? (if (or invert? (categorical? data y-col))
                                               bar-y-ticks?
                                               bar-x-ticks?)
                              #:label (and add-legend? (if-auto label y-col)))]
    [(struct* bars ([y-col y-col]
                    [invert? invert?]))
     (plot:rectangles (bars-data->rectangles raw-data invert?)
                      #:color (if-auto color (plot:rectangle-color))
                      #:line-color (if-auto color (plot:rectangle-color))
                      #:alpha (if-auto alpha (plot:rectangle-alpha))
                      #:line-width (if-auto size (plot:rectangle-line-width))
                      #:style (if-auto type (plot:rectangle-style))
                      #:label (and add-legend? (if-auto label y-col)))]
    [(struct* stacked-bars ([x-col x-col]
                            [group-col group-col]
                            [invert? invert?]
                            [labels? labels?]))
     (list (plot:stacked-histogram raw-data
                                   #:colors (if-auto color (plot:stacked-histogram-colors))
                                   #:alphas (list (if-auto alpha (plot:stacked-histogram-alphas)))
                                   #:line-widths (if-auto size (plot:stacked-histogram-line-widths))
                                   #:styles (if-auto type (plot:stacked-histogram-styles))
                                   #:invert? invert?
                                   #:add-ticks? (if invert?
                                                    bar-y-ticks?
                                                    bar-x-ticks?)
                                   #:labels (if add-legend?
                                                (if-auto label
                                                         (map ~a (group-ordering data group-col)))
                                                '(#f))
                                   )
           (if labels?
               (make-stacked-bar-labels (data->stacked-bar-label-info data
                                                                      raw-data
                                                                      x-col
                                                                      group-col)
                                        invert?)
               empty))]
    [(struct* stacked-area ([x-col x-col]
                            [group-col group-col]
                            [labels? labels?]))
     (define ->0 (match-lambda [(list x y) (list x 0)]))
     (define groups (group-ordering data group-col))
     (for/fold ([plot:renderers empty]
                [last-group-points #f]
                #:result plot:renderers)
               ([group (in-list groups)]
                [group-points (in-list raw-data)]
                [color (color-sequence)]
                [group-label (in-list (if-auto label groups))])
       (define this-renderer
         (plot:lines-interval (or last-group-points
                                  (list (->0 (argmin first group-points))
                                        (->0 (argmax first group-points))))
                              group-points
                              #:color color
                              #:line1-color color
                              #:line2-width 0
                              #:alpha (if-auto alpha (plot:interval-alpha))
                              #:label (and add-legend? (~a group-label))))
       (values (cons this-renderer plot:renderers)
               group-points))]
    [(struct* histogram ([col x-col]
                         [invert? invert?]))
     (plot:discrete-histogram raw-data
                              #:color (if-auto color (plot:rectangle-color))
                              #:alpha (if-auto alpha (plot:rectangle-alpha))
                              #:line-width (if-auto size (plot:rectangle-line-width))
                              #:style (if-auto type (plot:rectangle-style))
                              #:invert? invert?
                              #:add-ticks? (if invert?
                                               bar-y-ticks?
                                               bar-x-ticks?)
                              #:label (and add-legend? (or label (~a x-col " count"))))]
    [(struct* function ([f f]
                        [min min]
                        [max max]))
     (plot:function f
                    min
                    max
                    #:color (if-auto color (plot:line-color))
                    #:alpha (if-auto alpha (plot:line-alpha))
                    #:width (if-auto size (plot:line-width))
                    #:style (if-auto type (plot:line-style))
                    #:label (and add-legend? label))]
    [(? legend?) empty]
    [(struct* line-marker ([horizontal? horizontal?]
                           [location loc]))
     ((if horizontal? plot:hrule plot:vrule)
      loc
      #:color (if-auto color (plot:line-color))
      #:alpha (if-auto alpha (plot:line-alpha))
      #:width (if-auto size (plot:line-width))
      #:style (if-auto type (plot:line-style))
      #:label (and add-legend? (if-auto label #f)))]))

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

(define (renderer->plot:data unconverted-data
                             a-renderer
                             [converted-data (converter-transform unconverted-data a-renderer)])
  (define data converted-data)
  (match a-renderer
    [(struct* point-label ([x x] [y y]))
     (list x y)]
    [(or (struct* points ([x-col (? string? x-col)]
                          [y-col (? string? y-col)]))
         (struct* line ([x-col (? string? x-col)]
                        [y-col (? string? y-col)]))
         (struct* bars ([x-col (? string? x-col)]
                        [y-col (? string? y-col)])))
     (for/list ([x-y-vec (in-vector (df-select* data
                                                x-col
                                                y-col))])
       (vector->list x-y-vec))]
    #;[(points _ (? string? x-col) #f)
       (vector->list (df-select data x-col))]
    [(struct* stacked-bars ([x-col x-col]
                            [y-col y-col]
                            [group-col group-col]
                            [aggregator aggregator]))
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
                                       group-col))
     (define group-sequence (group-ordering sorted-data group-col))
     (for/list ([x-col-group-df (in-list (split-with sorted-data x-col))])
       (define x-value (any-value-in x-col-group-df x-col))
       (define group-col-dfs (split-with x-col-group-df group-col))
       (list x-value
             (for/list ([group (in-list group-sequence)])
               (define group-df (find-group-df group-col-dfs
                                               group-col
                                               group))
               (if group-df
                   (apply aggregator (vector->list (df-select group-df y-col)))
                   0))))]
    [(struct* stacked-area ([x-col x-col]
                            [y-col y-col]
                            [group-col group-col]
                            [aggregator aggregator]))
     (define group-sequence (group-ordering data group-col))
     (define grouped-points
       (data->grouped-points data x-col y-col group-col group-sequence aggregator))
     (check-aligned! grouped-points
                     a-renderer
                     group-sequence)
     (grouped-points->stacked-points grouped-points)]
    [(struct* histogram ([col col]
                         [bins bins]))
     (define the-values (vector->list (df-select data col)))
     (cond [(or (categorical? data col)
                (< (length (remove-duplicates the-values)) bins))
            (define frequencies
              (samples->hash the-values))
            (define histogram-data (hash-map frequencies list))
            (if (categorical? data col)
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
                    (length (sample-bin-values a-bin))))])]
    [(struct* function ([f f]
                        [min min]
                        [max max]))
     (for/list ([x (in-range min max (/ (- max min) (plot:line-samples)))])
       (list x (f x)))]
    [(? line-marker?) empty]))

;; data-frame? renderer? -> data-frame?
(define (converter-transform data a-renderer)
  (match-define (renderer _ (converters x-conv y-conv group-conv))
    a-renderer)
  (match a-renderer
    [(or (struct* points ([x-col (? string? x-col)]
                          [y-col (? string? y-col)]))
         (struct* line ([x-col (? string? x-col)]
                        [y-col (? string? y-col)]))
         (struct* bars ([x-col (? string? x-col)]
                        [y-col (? string? y-col)])))
     (if (or x-conv y-conv)
         (df-transform-series data
                              (for/list ([col (list x-col y-col)]
                                         [f   (list x-conv y-conv)]
                                         ;; skip doing work if there's no converter
                                         #:when f)
                                (list col f)))
         data)]
    [(or (struct* stacked-bars ([x-col x-col]
                            [y-col y-col]
                            [group-col group-col]))
         (struct* stacked-area ([x-col x-col]
                            [y-col y-col]
                            [group-col group-col])))
     #;(if (or x-conv y-conv group-conv)
         (rename
          (for/data-frame (X Y GROUP)
            ([{x y group} (in-data-frame data x-col y-col group-col)])
            (values (if x-conv (x-conv x) x)
                    (if y-conv (y-conv y) y)
                    (if group-conv (group-conv group) group)))
          "X" x-col
          "Y" y-col
          "GROUP" group-col)
         data)
     (if (or x-conv y-conv group-conv)
         (df-transform-series data
                              (for/list ([col (list x-col y-col group-col)]
                                         [f   (list x-conv y-conv group-conv)]
                                         ;; skip doing work if there's no converter
                                         #:when f)
                                (list col f)))
         data)]
    [(struct* histogram ([col col]
                         [bins bins]))
     (if x-conv
         (df-transform-series data (list (list col x-conv)))
         data)]
    [else data]))

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

;; data-frame? string? string? string?
;; ->
;; (listof points-list?)
;; where points-list? := (listof (list/c real? real?))
;; Each points-list in the result is sorted by first / x values.
(define (data->grouped-points data ;; assume that any converters have been applied already
                              x-col
                              y-col
                              group-col
                              groups
                              aggregator)
  (define group-datas (split-with data group-col))
  (for*/list ([group (in-list groups)]
              [group-data (in-value (find-group-df group-datas group-col group))])
    (define plain-points (renderer->plot:data group-data
                                              (make-points #:x x-col
                                                           #:y y-col)))
    (define sorted (sort plain-points < #:key first))
    (define aggregated
      (if (or (empty? sorted)
              (false? aggregator))
          sorted
          (aggregate-by-x-values sorted aggregator)))
    aggregated))

;; non-empty-list? (any/c ... -> any/c) -> non-empty-list?
(define (aggregate-by-x-values points aggregator)
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

(define (find-group-df df-partitions-by-group-col group-col group)
  (findf (λ (df) (equal? group (any-value-in df group-col)))
         df-partitions-by-group-col))

(define (any-value-in data col)
  (vector-ref (df-select data col) 0))

(define (sort-data-by-x-col-then-groups data x-col group-col)
  (define sorted-by-x (reorder data x-col))
  (define grouped-by-x (group-with sorted-by-x x-col))
  (define sorted-within-x (reorder grouped-by-x group-col))
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

(define (group-ordering data group-col [ordering orderable<?])
  (sort (remove-duplicates (vector->list (df-select data group-col)))
        ordering))

(define (data->stacked-bar-label-info data
                                      raw-data
                                      x-col
                                      group-col)
  (define group-sequence (group-ordering data group-col))
  (append*
   (for/list ([x-col-group    (in-list raw-data)]
              [x-pos          (in-range 0.5 (+ 0.5 (length raw-data)))])
     (for/fold ([points empty]
                [bar-height-so-far 0]
                #:result points)
               ([group-value (in-list group-sequence)]
                [y-value     (in-list (second x-col-group))]
                #:when (> y-value 0))
       (values (cons (list x-pos
                           (+ bar-height-so-far (/ y-value 2))
                           group-value)
                     points)
               (+ bar-height-so-far y-value))))))

(define (make-stacked-bar-labels label-info ;; as returned by `data->stacked-bar-label-info`
                                 invert?)
  (for/list ([info (in-list label-info)])
    (match-define (list x-pos y-pos label) info)
    (plot:point-label (if invert?
                          (list y-pos x-pos)
                          (list x-pos y-pos))
                      (~a label)
                      #:anchor 'center
                      #:point-size 0)))

(define (bar-plot? renderers)
  (and (not (empty? renderers))
       (andmap (disjoin bars? stacked-bars? histogram?) renderers)))


(struct p+l (point ; (or/c (list/c real? real?) real?)
                   ; ^ single real is a y-value,
                   ; means whatever the right edge of the plot is, use that for x
             label ; string?
             ))
(define (renderer->rightmost-points+labels data
                                           a-renderer)
  (define renderer-raw-data (renderer->plot:data data a-renderer))
  (define the-appearance (renderer-appearance a-renderer))
  (match a-renderer
    [(or (? line?) (? points?) (? function?) (? bars?))
     (define sorted-data (sort renderer-raw-data < #:key first))
     (list (p+l (last sorted-data)
                (if-auto (appearance-label the-appearance)
                         (renderer->y-axis-col a-renderer))))]
    [(and (struct* line-marker ([horizontal? #t]
                                [location y]))
          (app (compose1 appearance-label renderer-appearance)
               (and label (not #f))))
     (list (p+l y (if-auto label (~a y))))]
    [(? line-marker?) empty] ; vertical lines have no y-axis label
    [(struct* stacked-area ([group-col group-col]
                            [labels? labels?]))
     (define labels
       (cond [labels?
              (map ~a (group-ordering data group-col))]
             [(and (list? (appearance-label the-appearance))
                   (= (length renderer-raw-data)
                      (length (appearance-label the-appearance))))
              (appearance-label the-appearance)]
             [else
              (error 'complot
                     @~a{
                         Don't know how to label the stacked area plot.
                         Either enable automatic labeling with #:labels? #t
                         or provide a list of labels with #:label
                         which matches the number of distinct groups in
                         the data.
                         })]))
     (for/list ([points (in-list renderer-raw-data)]
                [label  (in-list labels)])
       (p+l (last points) ; leverage the sorting of points that happens in renderer->plot:data
            label))]))

(define renderer->y-axis-col
  (match-lambda [(or (struct* points ([y-col y]))
                     (struct* line ([y-col y]))
                     (struct* bars ([y-col y]))) y]
                [(? function?) "function"]))

(define complot-rectangle-width (make-parameter 0.8))
(define (bars-data->rectangles raw-bars-data invert?)
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
  (if invert?
      (invert-rectangles rectangles-data)
      rectangles-data))

(module+ test
  (require rackunit)
  (let ([sorted (sort-data-by-x-col-then-groups
                 (row-df [major minor money]
                         "expenses" "food" 20
                         "expenses" "transport" 30
                         "expenses" "laundry" 10
                         "expenses" "laundry" 5
                         "income" "paycheck" 100
                         "income" "side-job" 10)
                 "major"
                 "minor")])
    (check-equal? (sequence->list (in-data-frame/as-list sorted
                                                         "major"
                                                         "minor"
                                                         "money"))
                  '(("expenses" "food" 20)
                    ("expenses" "laundry" 10)
                    ("expenses" "laundry" 5)
                    ("expenses" "transport" 30)
                    ("income" "paycheck" 100)
                    ("income" "side-job" 10)))
    (check-equal? (group-ordering sorted "minor")
                  '("food" "laundry" "paycheck" "side-job" "transport")))
  (check-equal? (renderer->plot:data (row-df [major minor money]
                                             "expenses" "food" 20
                                             "expenses" "transport" 30
                                             "expenses" "laundry" 10
                                             "expenses" "laundry" 5
                                             "income" "paycheck" 100
                                             "income" "side-job" 10)
                                     (make-stacked-bars #:x "major"
                                                        #:group-by "minor"
                                                        #:y "money"))
                ;; order due to alphabetic sorting by `minor`, 0s fill missing values of `minor`
                '(("expenses" (20 15 0 0 30))
                  ("income" (0 0 100 10 0))))
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
                    (make-stacked-bars #:x "major"
                                       #:group-by "minor"
                                       #:y "money"
                                       #:labels? #t))
                   "major"
                   "minor"))
                '((0.5 50 "transport")
                  (0.5 55/2 "laundry")
                  (0.5 10 "food")
                  (1.5 105 "side-job")
                  (1.5 50 "paycheck")))

  (check-match (renderer->plot:data (row-df [date price ok?]
                                            1 20.50 "yes"
                                            2 22 "no"
                                            3 20 "no"
                                            4 23 "no"
                                            4 23 "yes"
                                            5 26.34 "kinda")
                                    (make-histogram #:x "date"))
               (list '(1 1)
                     '(2 1)
                     '(3 1)
                     '(4 2)
                     '(5 1)))
  (check-match (renderer->plot:data (row-df [date price ok?]
                                            1 20.50 "yes"
                                            2 22 "no"
                                            3 20 "no"
                                            4 23 "no"
                                            4 23 "yes"
                                            5 26.34 "kinda")
                                    (make-histogram #:x "price"))
               (list '(20 1)
                     '(20.50 1)
                     '(22 1)
                     '(23 2)
                     '(26.34 1)))
  (check-match (renderer->plot:data (row-df [day "price of A" "price of B"]
                                            1 10 15
                                            10 11 16
                                            11 12 17
                                            12 15 16
                                            13 17 16
                                            13 18 15
                                            14 25 20
                                            15 26 18
                                            16 11 16
                                            17 12 17
                                            18 15 16
                                            19 17 16
                                            19 18 15
                                            20 25 20
                                            30 26 18

                                            1 10 15
                                            10 11 16
                                            11 12 17
                                            12 15 16
                                            13 17 16
                                            13 18 15
                                            14 25 20
                                            15 26 18
                                            16 11 16
                                            17 12 17
                                            18 15 16
                                            19 17 16
                                            19 18 15
                                            20 25 20
                                            30 26 18
                                            )
                                    (make-histogram #:x "day"))
               (list '(1 2)
                     '(10 2)
                     '(11 2)
                     '(12 2)
                     '(13 4)
                     '(14 2)
                     '(15 2)
                     '(16 2)
                     '(17 2)
                     '(18 2)
                     '(19 4)
                     '(20 2)
                     '(30 2)))
  (check-equal? (renderer->plot:data (row-df [date price ok?]
                                             1 20.50 "yes"
                                             2 22 "no"
                                             3 20 "no"
                                             4 23 "no"
                                             4 23 "yes"
                                             5 26.34 "kinda")
                                     (make-points #:x "date" #:y "price"
                                                  #:x-converter (λ (d) (+ d 10))))
                '((11 20.50)
                  (12 22)
                  (13 20)
                  (14 23)
                  (14 23)
                  (15 26.34)))
  (check-equal? (grouped-points->stacked-points '(((1 2) (2 2) (3 4) (4 1))
                                                  ((1 2) (2 1) (3 5) (4 1))
                                                  ((1 1) (2 2) (3 7) (4 2))))
                '(((1 2) (2 2) (3 4) (4 1))
                  ((1 4) (2 3) (3 9) (4 2))
                  ((1 5) (2 5) (3 16) (4 4))))

  (check-equal? (data->grouped-points (row-df [date price ok?]
                                              1 20.50 "yes"
                                              2 22 "no"
                                              2 1 "no"
                                              3 20 "no"
                                              4 23 "no"
                                              4 23 "yes"
                                              5 26.34 "kinda")
                                      "date"
                                      "price"
                                      "ok?"
                                      '("yes" "no" "kinda")
                                      #f)
                '(((1 20.50) (4 23))
                  ((2 22) (2 1) (3 20) (4 23))
                  ((5 26.34))))
  (check-equal? (aggregate-by-x-values '((2 22) (2 1) (3 20) (3 0) (4 23))
                                       +)
                '((2 23) (3 20) (4 23)))
  (check-equal? (data->grouped-points (row-df [date price ok?]
                                              1 20.50 "yes"
                                              2 22 "no"
                                              2 1 "no"
                                              3 20 "no"
                                              4 23 "no"
                                              4 23 "yes"
                                              5 26.34 "kinda")
                                      "date"
                                      "price"
                                      "ok?"
                                      '("yes" "no" "kinda")
                                      +)
                '(((1 20.50) (4 23))
                  ((2 23) (3 20) (4 23))
                  ((5 26.34)))))
