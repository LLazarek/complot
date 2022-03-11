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
         (prefix-in pict: pict)
         sawzall
         data-frame
         file/convertible)

;; Function definition
;; `. things` is a rest-argument. E.g. (with x 1 2 3) binds '(1 2 3) to things.
(define (with a-plot . things)
  (define (with-one a-plot a-thing)
    (match a-thing ;; pattern matching
      ;; every clause has the shape [<pattern> <body>], and match tries to match
      ;; `a-thing` to each <pattern> (in turn), evaluating the <body> of the first
      ;; such clause that matches.

      ;; (? <predicate> pattern) is a pattern meaning: if the value satisfies <predicate>,
      ;; match it against pattern.
      ;; A pattern consisting of just an identifier means: match any value, and bind it
      ;; to the identifier.

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

;; The rendering function that turns a plot data structure into a visualization image.
(define (render-plot a-plot
                     ;; default argument
                     [outpath #f]
                     ;; keyword argument, kind of like Python's
                     #:width [width (plot:plot-width)]
                     #:height [height (plot:plot-height)])
  ;; match-define matches a value against a pattern, binding identifiers in the
  ;; pattern like `define`
  (match-define (plot data x-axis y-axis maybe-legend title renderers) a-plot)
  (match-define (list x-min x-max x-axis-plot:renderers)
    (if x-axis
        (x-axis->plot:axis x-axis data renderers)
        (list #f #f empty)))
  (match-define (list y-min y-max y-axis-plot:renderers)
    (if y-axis
        (y-axis->plot:axis y-axis data renderers)
        (list #f #f empty)))
  (define new-style-legend? (should-add-new-style-legend? maybe-legend renderers))
  (define plot:renderers
    (renderers->plot:renderer-tree data
                                   renderers
                                   #:bar-x-ticks? (not (empty? x-axis-plot:renderers))
                                   #:bar-y-ticks? (not (empty? y-axis-plot:renderers))
                                   #:legend? (and maybe-legend
                                                  (not new-style-legend?))))
  ;; Many customizable aspects of the racket plot library are controlled through "parameters",
  ;; which are racket's way of implementing dynamic binding (as opposed to lexical).
  ;; A parameter is basically a mutable cell whose value can be temporarily set during
  ;; the dynamic extent of a piece of code, and then reset upon exiting.
  ;; The parameterize form does this temporary setting.
  (parameterize (; Each clause has shape [<parameter> <value>]
                 [plot:plot-title           title]
                 [plot:plot-x-label         (axis->label x-axis)]
                 [plot:plot-y-label         (axis->label y-axis)]
                 [plot:plot-x-ticks         plot:no-ticks]
                 [plot:plot-y-ticks         plot:no-ticks]
                 [plot:plot-x-transform     (first (axis->ticks+transform x-axis))]
                 [plot:plot-y-transform     (first (axis->ticks+transform y-axis))]
                 [plot:plot-pen-color-map   'tab10]
                 [plot:plot-brush-color-map 'tab10])
    (define the-plot-pict
      ;; We boil down to racket plot's plotting function,
      ;; with various settings configured by the parameters above and keyword arguments below.
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
    ;; Racket's plot function produces a `pict`, which represents an image.
    ;; The pict library provides utilities for manipulating and combining images,
    ;; which we use to add the new-style-legend -- since racket's plot doesn't
    ;; support it.
    (define plot-pict+legend
      (cond [new-style-legend?
             (add-new-style-legend the-plot-pict a-plot)]
            [else the-plot-pict]))
    (if outpath
        ;; Convert the pict into a common image format (based on the extension of the path),
        ;; and write it to the file.
        (call-with-output-file outpath
          #:exists 'replace
          (λ (out) (write-bytes (convert plot-pict+legend
                                         (match outpath
                                           ;; The (regexp rx) pattern matches if
                                           ;; the value is a string matching rx.
                                           [(regexp #rx"\\.png$") 'png-bytes]
                                           [(regexp #rx"\\.gif$") 'gif-bytes]
                                           [(regexp #rx"\\.svg$") 'svg-bytes]
                                           [(regexp #rx"\\.pdf$") 'pdf-bytes]))
                                out)))
        plot-pict+legend)))

;; More than just plots can be rendered, and the easiest way to do that is to
;; add whatever it is to an empty plot and then render that!
(define (render thing [outpath #f])
  (match thing
    [(? plot? p) (render-plot p outpath)]
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
    [(and (or (point-label _ x-name y-name _ _)
              (points _ x-name y-name _)
              (line _ x-name y-name)
              (bars _ x-name y-name _))
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
    [(and (stacked-bars _ x-name group-name y-name _ _ _)
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
    [(and (histogram _ x-name _ _)
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
  ;; match* matches multiple values at once to patterns
  (match* {maybe-legend renderers}
    [{(legend 'auto 'new)
      (list (or (? line?) (? points?) (? function?)) ...)}
     #t]
    [{_ _} #f]))

;; Uses the pict library utilities to add the legend markers on the side of the
;; plot picture.
(define (add-new-style-legend plot-pict a-plot)
  (define data (plot-data a-plot))
  (define convert-coords (plot:plot-pict-plot->dc plot-pict))
  (define coords+labels
    (for/list ([renderer (in-list (plot-renderers a-plot))])
      (define renderer-raw-data (renderer->plot:data data renderer))
      (define sorted-data (sort renderer-raw-data < #:key first))
      (define rightmost-point (last sorted-data))
      (define dc-coords-of-rightmost-point
        (convert-coords (list->vector rightmost-point)))
      (define the-appearance (renderer-appearance renderer))
      (list (vector-ref dc-coords-of-rightmost-point 0)
            (vector-ref dc-coords-of-rightmost-point 1)
            (if-auto (appearance-label the-appearance)
                     (renderer->y-axis-col renderer))
            (if-auto (appearance-color the-appearance)
                     "black"))))
  (pict:panorama
   (for/fold ([plot-pict plot-pict])
             ([coords+label (in-list coords+labels)])
     (match-define (list x y label color) coords+label)
     (define label-pict
       (pict:text label
                  (plot:plot-font-family)
                  (round (* (plot:plot-font-size)
                            (new-legend-label-scale-factor)))))
     (define label-pict+color
       (if (colorize-new-legend-labels?)
           (pict:colorize label-pict
                          (match color
                            [(list* c _) c]
                            [other other]))
           label-pict))
     (pict:pin-over plot-pict
                    (+ x 5)
                    (- y 10)
                    label-pict+color))))

(define colorize-new-legend-labels? (make-parameter #f))

(define new-legend-label-scale-factor (make-parameter 1.4))

(define renderer->y-axis-col
  (match-lambda [(or (points _ _ y _)
                     (line _ _ y)) y]
                [(? function?) "function"]))

(define (read-data path)
  (df-read/csv path))
(define (describe data)
  (define col-names (df-series-names data))
  (displayln @~a{
                 Data frame with @(length col-names) columns and @(df-row-count data) rows.
                 Columns: @(string-join (map ~s col-names) ", ")
                 }))

;; module+ test creates a submodule, in this case for holding tests.
;; The submodule doesn't run unless specifically asked for.
;; These are rather poor tests since they require visual inspection.
;; todo: automate these tests.
(module+ test
  ;; row-df is a form for writing literal data tables
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
  (with (make-plot (row-df [date price ok?]
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


  (make-x-axis)
  (make-y-axis))
