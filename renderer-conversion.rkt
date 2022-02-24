#lang at-exp racket

(provide renderers->plot:renderer-tree
         renderer->plot:renderer-tree
         renderer->plot:data)

(require "structs.rkt"
         "util.rkt"
         (prefix-in plot: plot)
         data-frame
         math/statistics
         sawzall)

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

(module+ test
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
