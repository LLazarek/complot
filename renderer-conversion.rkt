#lang at-exp racket

(provide renderers->plot:renderer-tree
         renderer->plot:renderer-tree
         renderer->plot:data)

(require "structs.rkt"
         "util.rkt"
         "error-reporting.rkt"
         (prefix-in plot: plot)
         data-frame
         math/statistics
         sawzall)

(define (renderer->plot:renderer-tree data a-renderer
                                      #:bar-x-ticks? bar-x-ticks?
                                      #:bar-y-ticks? bar-y-ticks?
                                      #:legend? add-legend?)
  (define raw-data (renderer->plot:data data a-renderer))
  (check-plot:data-types! a-renderer data raw-data)
  (match-define (renderer (appearance color alpha size type label) _)
    a-renderer)
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
            (define groups (split-with data group-col))
            (for/list ([group-data (in-list groups)]
                       [group-color (in-sequences
                                     (if (list? color)
                                         color
                                         (list color))
                                     (in-naturals))])
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
                         #:label (and add-legend? (or label y-col)))])]
    [(struct* line ([y-col y-col]))
     (plot:lines raw-data
                 #:color (if-auto color (plot:line-color))
                 #:alpha (if-auto alpha (plot:line-alpha))
                 #:width (if-auto size (plot:line-width))
                 #:style (if-auto type (plot:line-style))
                 #:label (and add-legend? (or label y-col)))]
    [(struct* bars ([y-col y-col]
                    [invert? invert?]))
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
    [(? legend?) empty]))

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

(define (renderer->plot:data data a-renderer)
  (match-define (renderer _ (converters x-conv y-conv group-conv))
    a-renderer)
  (match a-renderer
    [(struct* point-label ([x x] [y y]))
     (list x y)]
    [(or (struct* points ([x-col (? string? x-col)]
                          [y-col (? string? y-col)]))
         (struct* line ([x-col (? string? x-col)]
                        [y-col (? string? y-col)]))
         (struct* bars ([x-col (? string? x-col)]
                        [y-col (? string? y-col)])))
     (define x-converter (or x-conv identity))
     (define y-converter (or y-conv identity))
     (vector->list
      (vector-map (if (or x-conv y-conv)
                      (match-lambda [(vector x y)
                                     (list (x-converter x)
                                           (y-converter y))])
                      vector->list)
                  (df-select* data
                              x-col
                              y-col)))]
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
     (define converted-data (if (or x-conv y-conv group-conv)
                                (rename
                                 (for/data-frame (X Y GROUP)
                                   ([{x y group} (in-data-frame data x-col y-col group-col)])
                                   (values (if x-conv (x-conv x) x)
                                           (if y-conv (y-conv y) y)
                                           (if group-conv (group-conv group) group)))
                                 "X" x-col
                                 "Y" y-col
                                 "GROUP" group-col)
                                data))
     (define sorted-data
       (sort-data-by-x-col-then-groups converted-data
                                       x-col
                                       group-col))
     (define group-sequence (group-ordering sorted-data group-col))
     (for/list ([x-col-group-df (in-list (split-with sorted-data x-col))])
       (define x-value (any-value-in x-col-group-df x-col))
       (define group-col-dfs (split-with x-col-group-df group-col))
       (list x-value
             (for/list ([group (in-list group-sequence)])
               (define group-df (findf (λ (df) (equal? group (any-value-in df group-col)))
                                       group-col-dfs))
               (if group-df
                   (apply aggregator (vector->list (df-select group-df y-col)))
                   0))))]
    [(struct* histogram ([col col]
                         [bins bins]))
     (define the-values (vector->list (df-select data col)))
     (define converted-values (if x-conv
                                  (map x-conv the-values)
                                  the-values))
     (cond [(or (categorical? data col)
                (< (length (remove-duplicates converted-values)) bins))
            (define frequencies
              (samples->hash converted-values))
            (define histogram-data (hash-map frequencies list))
            (if (categorical? data col)
                histogram-data
                (sort histogram-data < #:key first))]
           [else
            ;; todo: improvement, this should be able to bin based on the axis bounds?
            ;; or have a seperate argument to say the bounds of binning.
            (define values-min (apply min converted-values))
            (define values-max (apply max converted-values))
            (define bin-bounds (range values-min
                                      values-max
                                      (/ (- values-max values-min) bins)))
            (for/list ([a-bin (bin-samples bin-bounds <= converted-values)])
              (list (sample-bin-min a-bin)
                    (length (sample-bin-values a-bin))))])]
    [(struct* function ([f f]
                        [min min]
                        [max max]))
     (for/list ([x (in-range min max (/ (- max min) (plot:line-samples)))])
       (list x (f x)))]))

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

(define (group-ordering data group-col)
  (sort (remove-duplicates (vector->list (df-select data group-col)))
        orderable<?))

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
                     '(30 2))))
