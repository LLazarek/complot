#lang at-exp racket

(provide (rename-out [make-plot          plot]
                     [make-x-axis        x-axis]
                     [make-y-axis        y-axis]
                     [make-legend        legend]
                     [make-point-label   point-label]
                     [make-points        points]
                     [make-line          line]
                     [make-bars          bars]
                     [make-stacked-bars  stacked-bars]
                     [make-histogram     histogram]
                     [make-function      function])
         with
         describe
         read-data
         describe
         render)

(require "structs.rkt"
         "renderer-conversion.rkt"
         "axis-conversion.rkt"
         "util.rkt"
         (prefix-in plot: plot)
         (prefix-in graphite: graphite)
         (prefix-in pict: (only-in pict text))
         sawzall
         data-frame)

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

(define (render-plot a-plot [outpath #f]
                     #:width [width (plot:plot-width)]
                     #:height [height (plot:plot-height)])
  (match-define (plot data x-axis y-axis legend title renderers) a-plot)
  (match-define (list x-min x-max x-axis-plot:renderers)
    (if x-axis
        (x-axis->plot:axis x-axis data renderers)
        (list #f #f empty)))
  (match-define (list y-min y-max y-axis-plot:renderers)
    (if y-axis
        (y-axis->plot:axis y-axis data renderers)
        (list #f #f empty)))
  (define plot:renderers
    (renderers->plot:renderer-tree data
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
                                   (plot:plot-legend-anchor))
               #:width width
               #:height height
               #:out-file outpath)))

(define (render thing)
  (match thing
    [(? plot? p) (render-plot p)]
    [(? x-axis? a) (render-plot (with (make-plot (row-df [dummy] 0)) a)
                                #:height 40)]
    [(? y-axis? a) (render-plot (with (make-plot (row-df [dummy] 0)) a)
                                #:width 40)]
    [(? renderer? r) (render-plot (with (make-plot
                                         ;; some arbitrary data that will
                                         ;; illustrate the different kinds
                                         ;; of renderers
                                         (row-df [x y]
                                                 1 2
                                                 2 2
                                                 2 8
                                                 2 7
                                                 2 3
                                                 3 7
                                                 4 4
                                                 5 2
                                                 5 0
                                                 6 8
                                                 7 7
                                                 7 9
                                                 7 9
                                                 8 9
                                                 9 1
                                                 9 6
                                                 10 3))
                                        r))]
    [(? title? t) (pict:text t)]
    [(? legend?)  (raise-user-error
                   'complot
                   "Can't render a legend by itself: legends need a renderer to describe")]))

(define (read-data path)
  (df-read/csv path))
(define (describe data)
  (define col-names (df-series-names data))
  (displayln @~a{
                 Data frame with @(length col-names) columns and @(df-row-count data) rows.
                 Columns: @(string-join (map ~s col-names) ", ")
                 }))

(module+ test
  (begin0 (void)
    (render (make-plot (row-df [date price]
                               1 20.50
                               2 22
                               3 20
                               4 23
                               5 26.34)))
    (render (with (make-plot (row-df [date price]
                                     1 20.50
                                     2 22
                                     3 20
                                     4 23
                                     5 26.34))
                  (make-points #:x "date" #:y "price" #:alpha 1)
                  (make-x-axis #:min 0 #:max 5)
                  (make-y-axis #:min 0 #:max 30)))
    (render (with (make-plot (row-df [date price]
                                     1 20.50
                                     2 22
                                     3 20
                                     4 23
                                     5 26.34))
                  (make-points #:x "date" #:y "price" #:alpha 1)
                  (make-line #:x "date" #:y "date")
                  (make-y-axis #:min 0 #:max 30)))
    (render (with (make-plot (row-df [date price ok?]
                                     1 20.50 "yes"
                                     2 22 "no"
                                     3 20 "no"
                                     4 23 "no"
                                     4 23 "yes"
                                     5 26.34 "kinda"))
                  (make-histogram #:x "ok?")
                  (make-x-axis)
                  (make-y-axis)))
    (render (with (make-plot (row-df [date price ok?]
                                     1 20.50 "yes"
                                     2 22 "no"
                                     3 20 "no"
                                     4 23 "no"
                                     4 23 "yes"
                                     5 26.34 "kinda"))
                  (make-histogram #:x "ok?")
                  (make-x-axis)
                  (make-y-axis #:min 0 #:major-tick-every 1 #:minor-ticks-between-major 0)))
    (render (with (make-plot (row-df [date price ok?]
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
    (render (with (make-plot (row-df [major minor money]
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
    (render (with (make-plot (row-df [major minor money]
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
    (render (with (make-plot (row-df [a] 5))
                  (make-function (λ (x) (expt 2 x))
                                 #:min 1 #:max 100)
                  (make-x-axis #:min 1 #:max 100)
                  (make-y-axis #:layout 'log))))


  (render (make-x-axis))
  (render (make-y-axis)))
