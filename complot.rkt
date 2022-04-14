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
                     [make-stacked-area  stacked-area]
                     [make-histogram     histogram]
                     [make-function      function]

                     [with               add-to]
                     [render             save-to-file])
         title
         with
         describe
         read-data
         describe
         render)

(require "structs.rkt"
         "renderer-conversion.rkt"
         "axis-conversion.rkt"
         "util.rkt"
         "error-reporting.rkt"
         (prefix-in plot: plot)
         (prefix-in plot: plot/utils)
         (prefix-in pict: pict)
         sawzall
         data-frame
         file/convertible)

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
       (plot-update a-plot renderers snoc r)]

      [something-else
       (report-non-complot-argument! a-plot something-else "add-to (aka with)")]))
  (unless (plot? a-plot)
    (report-non-complot-argument! a-plot #f "add-to (aka with)"))
  (match things
    [(cons a-thing more)
     (apply with (with-one a-plot a-thing) more)]
    ['() a-plot]))

(define current-complot-color-map (make-parameter 'tab10))

(define (render-plot a-plot
                     [outpath #f]
                     #:width [width (plot:plot-width)]
                     #:height [height (plot:plot-height)])
  (match-define (plot data x-axis y-axis maybe-legend title renderers) a-plot)
  (match-define (list x-min x-max x-ticks x-axis-plot:renderers add-bar-x-ticks?)
    (if x-axis
        (x-axis->plot:axis x-axis data renderers)
        (list #f #f #f empty #f)))
  (match-define (list y-min y-max y-ticks y-axis-plot:renderers add-bar-y-ticks?)
    (if y-axis
        (y-axis->plot:axis y-axis data renderers)
        (list #f #f #f empty #f)))
  (define new-style-legend? (should-add-new-style-legend? maybe-legend renderers))
  (define plot:renderers
    (renderers->plot:renderer-tree data
                                   renderers
                                   #:bar-x-ticks? add-bar-x-ticks?
                                   #:bar-y-ticks? add-bar-y-ticks?
                                   #:legend? (and maybe-legend
                                                  (not new-style-legend?))))
  (parameterize ([plot:plot-title           title]
                 [plot:plot-x-label         (axis->label x-axis)]
                 [plot:plot-y-label         (axis->label y-axis)]
                 [plot:plot-x-ticks         (or x-ticks plot:no-ticks)]
                 [plot:plot-y-ticks         (or y-ticks plot:no-ticks)]
                 [plot:plot-x-transform     (first (axis->transform+ticks x-axis))]
                 [plot:plot-y-transform     (first (axis->transform+ticks y-axis))]
                 [plot:plot-pen-color-map   (current-complot-color-map)]
                 [plot:plot-brush-color-map (current-complot-color-map)])
    (define the-plot-pict
      (plot:plot-pict (append (list x-axis-plot:renderers
                                    y-axis-plot:renderers)
                              plot:renderers)
                      #:x-min (or x-min (and (empty? plot:renderers) 0))
                      #:x-max (or x-max (and (empty? plot:renderers) 0))
                      #:y-min (or y-min (and (empty? plot:renderers) 0))
                      #:y-max (or y-max (and (empty? plot:renderers) 0))
                      #:legend-anchor (match maybe-legend
                                        [(legend (and pos (not 'auto)) 'old) pos]
                                        [else (plot:plot-legend-anchor)])
                      #:width width
                      #:height height))
    (define plot-pict+legend
      (cond [new-style-legend?
             (add-new-style-legend the-plot-pict a-plot)]
            [else the-plot-pict]))
    (if outpath
        (call-with-output-file outpath
          #:exists 'replace
          (λ (out) (write-bytes (convert plot-pict+legend
                                         (match outpath
                                           [(regexp #rx"\\.png$") 'png-bytes]
                                           [(regexp #rx"\\.gif$") 'gif-bytes]
                                           [(regexp #rx"\\.svg$") 'svg-bytes]
                                           [(regexp #rx"\\.pdf$") 'pdf-bytes]))
                                out)))
        plot-pict+legend)))

(define (render thing [outpath #f]
                #:width [width (plot:plot-width)]
                #:height [height (plot:plot-height)])
  (match thing
    [(? plot? p) (render-plot p outpath
                              #:width width
                              #:height height)]
    [(? x-axis? a) (render-plot (with (make-plot (row-df [x y]
                                                         0 0
                                                         1 0))
                                      (make-points #:x "x" #:y "y" #:alpha 0)
                                      a)
                                outpath
                                #:height 40)]
    [(? y-axis? a) (render-plot (with (with (make-plot (row-df [x y]
                                                         0 0
                                                         0 1))
                                      (make-points #:x "x" #:y "y" #:alpha 0)
                                      a) a)
                                outpath
                                #:width 40)]
    [(and (or (struct* point-label ([x x-name] [y y-name]))
              (struct* points ([x-col x-name] [y-col y-name]) )
              (struct* line ([x-col x-name] [y-col y-name]) )
              (struct* bars ([x-col x-name] [y-col y-name]) ))
          r)
     (render-plot (with (make-plot
                         ;; some arbitrary data that will
                         ;; illustrate the different kinds
                         ;; of renderers
                         (row-df [(~a x-name) (~a y-name)]
                                 1 2
                                 2 2
                                 3 7
                                 4 4
                                 5 2
                                 6 8
                                 7 9
                                 8 9
                                 9 1
                                 10 3))
                        r)
                  outpath)]
    [(and (or (struct* stacked-bars ([x-col x-name]
                                     [group-col group-name]
                                     [y-col y-name]))
              (struct* stacked-area ([x-col x-name]
                                     [group-col group-name]
                                     [y-col y-name])))
          r)
     (render-plot (with (make-plot
                         ;; some arbitrary data that will
                         ;; illustrate the different kinds
                         ;; of renderers
                         (row-df [(~a x-name) (~a y-name) (~a group-name)]
                                 1 2 "A"
                                 2 2 "A"
                                 3 7 "A"
                                 1 1 "B"
                                 2 5 "B"
                                 3 4 "B"
                                 ))
                        r)
                  outpath)]
    [(and (struct* histogram ([col x-name]))
          r)
     (render-plot (with (make-plot
                         ;; some arbitrary data that will
                         ;; illustrate the different kinds
                         ;; of renderers
                         (column-df [(~a x-name) (list->vector
                                                  (for/list ([i (in-range 100)])
                                                    (random 100)))]))
                        r)
                  outpath)]
    [(? function? r)
     (render-plot (with (make-plot
                         ;; some arbitrary data that will
                         ;; illustrate the different kinds
                         ;; of renderers
                         (row-df [x] 1))
                        r)
                  outpath)]
    [(? title? t) (pict:text t)]
    [(? legend?)  (raise-user-error
                   'complot
                   "Can't render a legend by itself: legends need a renderer to describe")]
    [something-else
     (report-non-complot-argument! (make-plot #f)
                                   something-else
                                   "save-to-file (aka render)")]))



;; A little bit of trickery here to support rendering plots and elements in the REPL.
;; `structs.rkt` defines the parameter `current-complot-printer` to hold the function
;; which "prints" complot data structures.
;; The default function used in `structs.rkt` just prints some filler.
;; Here, we overwrite that default to call `render` instead.
;;
;; Going through this parameter is effectively using state to break the
;; circularity of `render` needing the structs module, and the struct printer
;; needing to be defined alongside the data structures in the structs module,
;; so it can't refer directly to `render`.
;;
;; Finally, we need a second parameter to prevent an infinite error loop if
;; an error should ever arise in the process of trying to print a complot thing,
;; and that error tries to print out the same complot thing.
(define currently-printing-complot-thing? (make-parameter #f))
(current-complot-printer (λ (thing port mode)
                           ;; todo: there's a bug where in the REPL a single
                           ;; plot is often being rendered twice and the first
                           ;; one being thrown away. It doesn't affect behavior
                           ;; but is strange, and haven't had time to hunt it down.
                           (define recur
                             (match mode
                               [#t write]
                               [#f display]
                               [else (λ (p port) (print p port mode))]))
                           (cond [(and #;(port-writes-special? port)
                                       ;; ^ rendering in racket-mode repl seems to not want
                                       ;; this condition
                                       mode
                                       (not (currently-printing-complot-thing?)))
                                  (parameterize ([currently-printing-complot-thing? #t])
                                    (recur (render thing) port))]
                                 [else (recur @~a{#<complot @(object-name thing)>}
                                              port)])))


(define (snoc x l) (append l (list x)))

(define (should-add-new-style-legend? maybe-legend renderers)
  (match* {maybe-legend renderers}
    [{(legend 'auto 'new)
      (list (or (? line?) (? points?) (? function?) (? stacked-area?)) ...)}
     #t]
    [{_ _} #f]))

(define (add-new-style-legend plot-pict a-plot)
  (define (string->label-pict label)
    (pict:text label
               (plot:plot-font-family)
               (round (* (plot:plot-font-size)
                         (new-legend-label-scale-factor)))))
  (define (make-label-with-marker label [y-offset #f] [x-offset #f])
    (define line-width 0.5)
    (define label-p (if (string? label)
                        (string->label-pict label)
                        label))
    (cond [(and y-offset x-offset)
           ;; x-offset
           ;; --------+
           ;;         |
           ;;         | y-offset
           ;;         |
           ;;         +- label
           (pict:ht-append (pict:filled-rectangle x-offset line-width
                                                  #:color "black")
                           (pict:vl-append (- (/ (pict:pict-height label-p) 2))
                                           (pict:filled-rectangle line-width y-offset
                                                                  #:color "black")
                                           (pict:hc-append 5 (pict:filled-rectangle 5 line-width
                                                                                    #:color "black")
                                                           label-p)))]
          [else label-p]))
  (define (fix-overlaps coords+labels)
    (for/fold ([current-bottom 0]
               [fixed empty]
               [x-offset 5]
               #:result fixed)
              ([coord+label (in-list (sort coords+labels < #:key second))])
      ;; Assuming that y grows down like so:
      ;; +--⟶ x
      ;; |
      ;; ⇓
      ;; y
      (match-define (list x y label color) coord+label)
      (define δ (max (- current-bottom y) 0))
      (define label-pict (make-label-with-marker label δ x-offset))
      (define new-offset (if (zero? δ)
                             5
                             (+ x-offset 5)))
      (values (+ y (pict:pict-height label-pict) 10)
              (cons (list x y label-pict color) fixed)
              new-offset)))

  (define data (plot-data a-plot))
  (define convert-coords (plot:plot-pict-plot->dc plot-pict))
  (define coords+labels
    (for/list ([renderer (in-list (plot-renderers a-plot))]
               #:when #t
               [a-p+l (in-list (renderer->rightmost-points+labels data renderer))]
               [color (match (appearance-color (renderer-appearance renderer))
                        ['auto (in-naturals)]
                        [(? list? l) (in-cycle l)]
                        [single (in-cycle (list single))])])
      (match-define (p+l rightmost-point label) a-p+l)
      (define dc-coords-of-rightmost-point
        (convert-coords (list->vector rightmost-point)))
      (list (vector-ref dc-coords-of-rightmost-point 0)
            (vector-ref dc-coords-of-rightmost-point 1)
            label
            (match color
              [(? integer?) (plot:->brush-color color)]
              [other other]))))
  (define coords+labels/non-overlapping
    (fix-overlaps coords+labels))
  (pict:panorama
   (for/fold ([plot-pict plot-pict])
             ([coords+label (in-list coords+labels/non-overlapping)]
              [i (in-naturals)])
     (match-define (list x y label-pict color) coords+label)
     (define label-pict+color
       (if (colorize-new-legend-labels?)
           (pict:colorize label-pict
                          color)
           label-pict))
     (pict:pin-over plot-pict
                    (+ x 5)
                    y
                    label-pict+color))))

(define colorize-new-legend-labels? (make-parameter #t))

(define new-legend-label-scale-factor (make-parameter 1.4))

(define (read-data path)
  (df-read/csv path))
(define (describe data)
  (define col-names (df-series-names data))
  (displayln @~a{
                 Data frame with @(length col-names) columns and @(df-row-count data) rows.
                 Columns: @(string-join (map ~s col-names) ", ")
                 }))

;; These are rather poor tests since they require visual inspection.
;; todo: automate these tests.
(module+ test
  (make-plot (row-df [date price]
                     1 20.50
                     2 22
                     3 20
                     4 23
                     5 26.34))
  (with (make-plot (row-df [date price]
                           1 20.50
                           2 22
                           3 20
                           4 23
                           5 26.34))
        (make-points #:x "date" #:y "price" #:alpha 1)
        (make-x-axis #:min 0 #:max 5)
        (make-y-axis #:min 0 #:max 30))
  (with (make-plot (row-df [date price]
                           1 20.50
                           2 22
                           3 20
                           4 23
                           5 26.34))
        (make-points #:x "date" #:y "price" #:alpha 1)
        (make-line #:x "date" #:y "date")
        (make-y-axis #:min 0 #:max 30))
  (with (make-plot (row-df [date price ok?]
                           1 20.50 "yes"
                           2 22 "no"
                           3 20 "no"
                           4 23 "no"
                           4 23 "yes"
                           5 26.34 "kinda"))
        (make-histogram #:x "ok?")
        (make-x-axis)
        (make-y-axis))
  ;; todo: waiting on support for these tick manipulating options
  #;(with (make-plot (row-df [date price ok?]
                           1 20.50 "yes"
                           2 22 "no"
                           3 20 "no"
                           4 23 "no"
                           4 23 "yes"
                           5 26.34 "kinda"))
        (make-histogram #:x "ok?")
        (make-x-axis)
        (make-y-axis #:min 0 #:major-tick-every 1 #:minor-ticks-between-major 0))
  (with (make-plot (row-df [date price ok?]
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
                     #:ensure-min-tick? #f))
  (with (make-plot (row-df [major minor money]
                           "expenses" "food" 20
                           "expenses" "transport" 30
                           "expenses" "laundry" 10
                           "expenses" "laundry" 5
                           "income" "paycheck" 100
                           "income" "side-job" 10))
        (make-stacked-bars #:x "major"
                           #:group-by "minor"
                           #:y "money"
                           #:labels? #f)
        (make-x-axis)
        (make-y-axis #:min 0))
  (with (make-plot (row-df [major minor money]
                           "expenses" "food" 20
                           "expenses" "transport" 30
                           "expenses" "laundry" 10
                           "expenses" "laundry" 5
                           "income" "paycheck" 100
                           "income" "side-job" 10))
        (make-stacked-bars #:x "major"
                           #:group-by "minor"
                           #:y "money")
        (make-x-axis)
        (make-y-axis #:min 0))
  (with (make-plot (row-df [a] 5))
        (make-function (λ (x) (expt 2 x))
                       #:min 1 #:max 100)
        (make-x-axis #:min 1 #:max 100)
        (make-y-axis #:layout 'log))

  (with (make-plot (row-df [x y y2]
                           1 5 2
                           2 3 2
                           3 6 5))
        (make-x-axis #:min 0)
        (make-y-axis #:min 0 #:max 10)
        (make-bars #:x "x" #:y "y2" #:label "gas")
        (make-legend #:type 'new))
  (render (with (make-plot (row-df [x y y2]
                                   1 5 2
                                   2 3 2
                                   3 6 5))
                (make-x-axis #:min 0)
                (make-y-axis #:min 0 #:max 10)
                (make-line #:x "x" #:y "y2" #:label "gas")
                (make-legend #:type 'new)))

  (render (with (make-plot (row-df [date price categ]
                                   1 20.50 1
                                   2 22 2
                                   3 20 2
                                   4 23 1
                                   5 26.34 2))
                (make-bars #:x "date" #:y "price")
                (make-y-axis)))
  (render (with (make-plot (row-df [date price categ]
                                   1 20.50 1
                                   2 22 2
                                   3 20 2
                                   4 23 1
                                   5 26.34 2))
                (make-bars #:x "date" #:y "price" #:invert? #t)
                (make-y-axis)
                (make-x-axis #:max 30)
                (make-legend #:type 'old)))
  (render (with (make-plot (row-df [date price categ]
                             1 20.50 1
                             1 22 2
                             2 20 2
                             2 23 1
                             2 26.34 2))
          (make-stacked-bars #:x "date" #:y "price" #:group-by "categ")
          (make-y-axis)
          (make-x-axis)
          (make-legend #:type 'old)))
  (render (with (make-plot (row-df [date price categ]
                             1 20.50 1
                             1 22 2
                             2 20 2
                             2 23 1
                             2 26.34 2))
          (make-stacked-bars #:x "date" #:y "price" #:group-by "categ" #:invert? #t)
          (make-y-axis)
          (make-x-axis)
          (make-legend #:type 'old)))

  (render (with (make-plot (row-df [date price ok?]
                              1 20.50 "yes"
                              2 22 "no"
                              3 20 "no"
                              4 23 "no"
                              4 23 "yes"
                              5 26.34 "kinda"))
                (make-histogram #:x "date")
                (make-x-axis)
                (make-y-axis)))

  (require gregor)
  (render (with (make-plot (row-df [date price]
                                   "2017" 20
                                   "2018" 30
                                   "2019" 100))
                (make-x-axis #:layout 'date #:ensure-max-tick? #f)
                (make-y-axis)
                (make-line #:x "date" #:y "price"
                           #:x-converter (λ (d) (->posix (parse-date d "y"))))))

  (render (with (make-plot (row-df [date y group]
                                   "2021-01-05" 5 1
                                   "2021-01-05" 10 2
                                   "2021-01-05" 15 2
                                   "2021-01-06" 5 1
                                   "2021-01-06" 10 2
                                   "2021-01-07" 20 2))
                (make-stacked-bars #:x "date" #:y "y" #:group-by "group")))

  (make-x-axis)
  (make-y-axis))
