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

(define (renderer->plot:renderer-tree data renderer
                                      #:bar-x-ticks? bar-x-ticks?
                                      #:bar-y-ticks? bar-y-ticks?
                                      #:legend? add-legend?)
  (define raw-data (renderer->plot:data data renderer))
  (check-plot:data-types! renderer data raw-data)
  (match renderer
    [(point-label (appearance color alpha size type label) x y content anchor)
     (plot:point-label raw-data
                       content
                       #:color (if-auto color (plot:plot-foreground))
                       #:alpha (if-auto alpha (plot:label-alpha))
                       #:point-size (if-auto size (plot:label-point-size))
                       #:point-sym (if-auto type 'fullcircle)
                       #:anchor anchor)]
    [(points (appearance color alpha size type label) x-col y-col group-col)
     (cond [group-col
            (define groups (split-with data group-col))
            (for/list ([group-data (in-list groups)]
                       [group-color (in-sequences
                                     (if (list? color)
                                         color
                                         (list color))
                                     (in-naturals))])
              (define group-col-value (df-select group-data group-col))
              (plot:points (renderer->plot:data group-data renderer)
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
    [(line (appearance color alpha size type label) x-col y-col)
     (plot:lines raw-data
                 #:color (if-auto color (plot:line-color))
                 #:alpha (if-auto alpha (plot:line-alpha))
                 #:width (if-auto size (plot:line-width))
                 #:style (if-auto type (plot:line-style))
                 #:label (and add-legend? (or label y-col)))]
    [(bars (appearance color alpha size type label) x-col y-col invert?)
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
    [(stacked-bars (appearance color alpha size type labels)
                   major-col
                   minor-col
                   value-col
                   invert?
                   _
                   labels?)
     (list (plot:stacked-histogram raw-data
                                   #:colors (if-auto color (plot:stacked-histogram-colors))
                                   #:alphas (list (if-auto alpha (plot:stacked-histogram-alphas)))
                                   #:line-widths (if-auto size (plot:stacked-histogram-line-widths))
                                   #:styles (if-auto type (plot:stacked-histogram-styles))
                                   #:invert? invert?
                                   #:add-ticks? (if invert?
                                                    bar-y-ticks?
                                                    bar-x-ticks?)
                                   #:labels (if legend?
                                                (if-auto labels '(#f)) ;; lltodo
                                                '(#f))
                                   )
           (if labels?
               (make-stacked-bar-labels data
                                        raw-data
                                        major-col
                                        minor-col
                                        invert?)
               empty))]
    [(histogram (appearance color alpha size type label) x-col bins invert?)
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
    [(function (appearance color alpha size type label) f min max)
     (plot:function f
                    min
                    max
                    #:color (if-auto color (plot:line-color))
                    #:alpha (if-auto alpha (plot:line-alpha))
                    #:width (if-auto size (plot:line-width))
                    #:style (if-auto type (plot:line-style))
                    #:label (and add-legend? label))]
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

(define (renderer->plot:data data renderer)
  (match renderer
    [(point-label _ x y _ _)
     (list x y)]
    [(or (points _ (? string? x-col) (? string? y-col) _)
         (line _ (? string? x-col) (? string? y-col))
         (bars _ (? string? x-col) (? string? y-col) _))
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
    [(function _ f min max)
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
                                 minor-col
                                 invert?)
  (for/list ([info (in-list (data->stacked-bar-label-info data raw-data major-col minor-col))])
    (match-define (list x-pos y-pos label) info)
    (plot:point-label (if invert?
                          (list y-pos x-pos)
                          (list x-pos y-pos))
                      (~a label)
                      #:anchor 'center
                      #:point-size 0)))

(module+ test
  (require rackunit)
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
                ;; 15 before 30 because of alphabetic sorting by `minor`
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
