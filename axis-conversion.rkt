#lang at-exp racket

(provide x-axis->plot:axis
         y-axis->plot:axis
         axis->label
         axis->transform+ticks
         infer-raw-data-bounds)

(require "structs.rkt"
         "renderer-conversion.rkt"
         (prefix-in plot: plot))

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
  (define-values {the-min the-max this-axis-is-categorical?}
    (cond [(and (axis-min an-axis)
                (axis-max an-axis))
           ;; Avoid calculating any inferred bounds if not needed
           (values (axis-min an-axis) (axis-max an-axis) #f)]
          [else
           (infer-bounds data
                         renderers
                         (x-axis? an-axis)
                         (axis-min an-axis)
                         (axis-max an-axis))]))
  (define extra-ticks
    (append (if (and (axis-ensure-min-tick? an-axis)
                     (not this-axis-is-categorical?))
                (list the-min)
                empty)
            (if (and (axis-ensure-max-tick? an-axis)
                     (not this-axis-is-categorical?))
                (list the-max)
                empty)
            (axis-minimum-ticks an-axis)))
  (define plot:ticks-thing (second (axis->transform+ticks an-axis)))
  (define plot-ticks
    (cond [(not (axis-ticks? an-axis))
           plot:no-ticks]
          [(not (empty? extra-ticks))
           (plot:ticks-add plot:ticks-thing
                           extra-ticks)]
          [else plot:ticks-thing])
    #;(if (and (axis-ticks? an-axis)
             (not this-axis-is-categorical?))
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
        ;; either this is a bar chart and we need the axis, but we
        ;; leave it up to plot's internal handling in render, or
        ;; we don't need the ticks at all
        empty))
  (define add-bar-ticks?
    (and (axis-ticks? an-axis)
         (bar-plot? renderers)
         (not (equal? (plot:ticks-layout plot:ticks-thing)
                      plot:no-ticks-layout))))
  (list the-min
        the-max
        plot-ticks
        (if (axis-tick-lines? an-axis)
            (if (x-axis? an-axis)
                (plot:x-tick-lines)
                (plot:y-tick-lines))
            empty)
        add-bar-ticks?
        #;(list (make-plot:axis plot-ticks)
              (if (axis-tick-lines? an-axis)
                  (if (x-axis? an-axis)
                      (plot:x-tick-lines)
                      (plot:y-tick-lines))
                  empty))))

(define (axis->transform+ticks axis)
  (define plot:transform
    (match (and axis (axis-layout axis))
      [(or #f 'auto) (plot:plot-x-transform)]
      ['log plot:log-transform]
      [other plot:id-transform]))
  (define plot:layout
    (match* {(and axis (axis-layout axis)) (and axis (axis-major-ticks-every axis))}
      [{(or #f 'auto 'linear) 'auto} (plot:linear-ticks-layout)]
      [{'log 'auto} (plot:log-ticks-layout)]
      [{'date 'auto} (plot:date-ticks-layout)]
      [{_ (or 0 #f)} plot:no-ticks-layout]
      [{_ (? number?)}
       (raise-user-error
        'x-/y-axis
        "#:major-tick-every only supports 0, #f, or 'auto for now.")]))
  (define plot:format
    (match (and axis (axis-layout axis))
      [(or #f 'auto 'linear) (plot:linear-ticks-format)]
      ['log (plot:log-ticks-format)]
      ['date (plot:date-ticks-format)]
      [other other]))
  (list plot:transform
        (plot:ticks plot:layout plot:format)))

(define inverted-renderer?
  (match-lambda [(or (struct* bars ([invert? invert?]))
                     (struct* stacked-bars ([invert? invert?]))
                     (struct* histogram ([invert? invert?])))
                 invert?]
                [else #f]))

(define (infer-raw-data-bounds data renderers x-axis? #|otherwise y|#)
  (define (real-min/maxes vs)
    (list (apply min vs) (apply max vs)))
  (define mixed-mins+maxes
    (for/list ([renderer (in-list renderers)])
      (match (renderer->plot:data data renderer)
        [(list x y)
         (if x-axis?
             (list x x)
             (list y y))]
        ['() (list #f #f)]
        [(list (list (? real? xs) _) ...)
         #:when x-axis?
         (real-min/maxes xs)]
        [(list (list _ (? real? ys)) ...)
         #:when (not x-axis?)
         (real-min/maxes ys)]
        [(list (list (? (negate real?) xs) _) ...)
         #:when x-axis?
         (list 0 (length xs))]
        [(list (list _ (list (? real? y-part-lists) ...)) ...)
         #:when (not x-axis?)
         (define y-totals
           (for/list ([y-parts (in-list y-part-lists)])
             (apply + y-parts)))
         (real-min/maxes y-totals)]
        [(list (list _ (? (negate real?) ys)) ...)
         #:when (not x-axis?)
         (list 0 (length ys))]
        [(list (list (list (? real? xs-lists) _) ...) ...)
         #:when x-axis?
         (real-min/maxes (flatten xs-lists))]
        [(list (list (list _ (? real? ys-lists)) ...) ...)
         #:when (not x-axis?)
         (real-min/maxes (flatten ys-lists))])))
  (define mins+maxes
    (filter (match-lambda [(list #f #f) #f] [else #t])
            mixed-mins+maxes))
  (if (empty? mins+maxes)
      (values #f #f)
      (values (apply min (map first mins+maxes))
              (apply max (map second mins+maxes)))))

(define (infer-bounds data renderers x-axis? user-min user-max)
  (define y-axis? (not x-axis?))
  (define this-is-a-bar-plot? (bar-plot? renderers))
  (define inverted-bar-plot?
    (and this-is-a-bar-plot?
         (andmap inverted-renderer? renderers)))
  (define-values {inferred-min inferred-max}
    (infer-raw-data-bounds data
                           renderers
                           (if inverted-bar-plot?
                               y-axis?
                               x-axis?)))
  (define this-axis-is-categories-of-bar-plot?
    (and this-is-a-bar-plot?
         (if inverted-bar-plot?
             y-axis?
             x-axis?)))
  (define this-axis-is-values-of-bar-or-area-plot?
    (and (or this-is-a-bar-plot?
             (andmap stacked-area? renderers))
         (if inverted-bar-plot?
             x-axis?
             y-axis?)))
  (values (cond [user-min => values]
                [this-axis-is-categories-of-bar-plot? #f]
                [this-axis-is-values-of-bar-or-area-plot? (min inferred-min 0)]
                ;; If can't even infer bounds, just use dummy bounds of [0, 1]
                [else (or inferred-min 0)])
          (cond [user-max => values]
                [this-axis-is-categories-of-bar-plot? #f]
                ;; ... ditto above
                [else (or inferred-max 1)])
          this-axis-is-categories-of-bar-plot?))


(module+ test
  (require rackunit
           syntax/parse/define
           sawzall)
  (define-simple-macro (values->list e)
    (call-with-values (thunk e) list))
  (define-check (check-infer-bounds data renderers x-bounds y-bounds)
    (with-check-info (['bound "x"])
      (define actual-x-bounds (drop-right (values->list (infer-bounds data renderers #t #f #f)) 1))
      (unless (equal? actual-x-bounds x-bounds)
        (fail-check @~a{actual: @~s[actual-x-bounds]})))
    (with-check-info (['bound "y"])
      (define actual-y-bounds (drop-right (values->list (infer-bounds data renderers #f #f #f)) 1))
      (unless (equal? actual-y-bounds y-bounds)
        (fail-check @~a{actual: @~s[actual-y-bounds]}))))
  (check-equal? (values->list (infer-bounds (row-df [x y z]
                                                    1 10 5
                                                    2 15 7
                                                    5 8 10)
                                            empty
                                            #t
                                            #f
                                            #f))
                (list 0 1 #f))
  (for ([renderer (list (make-points #:x "x" #:y "y")
                        (make-line #:x "x" #:y "y"))])
    (with-check-info (['renderer (~a renderer)])
      (check-infer-bounds (row-df [x y z]
                                  1 10 5
                                  2 15 7
                                  5 8 10)
                          (list renderer)
                          (list 1 5)
                          (list 8 15))))
  (check-infer-bounds (row-df [x y z]
                              1 10 5
                              2 15 7
                              5 8 10)
                      (list (make-bars #:x "x"
                                       #:y "y"))
                      (list #f #f)
                      (list 0 15))
  (check-infer-bounds (row-df [x y z]
                              1 10 5
                              2 15 7
                              5 8 10)
                      (list (make-bars #:x "x"
                                       #:y "y"
                                       #:invert? #t))
                      (list 0 15)
                      (list #f #f))
  (check-infer-bounds (row-df [x y z]
                              1 10 5
                              1 1 2
                              2 15 5
                              2 2 2
                              5 8 5
                              5 5 2)
                      (list (make-stacked-bars #:x "x"
                                               #:y "y"
                                               #:group-by "z"))
                      (list #f #f)
                      (list 0 17))
  (check-infer-bounds (row-df [x y z]
                              1 10 5
                              1 1 2
                              2 15 5
                              2 2 2
                              5 8 5
                              5 5 2)
                      (list (make-stacked-bars #:x "x"
                                               #:y "y"
                                               #:group-by "z"
                                               #:invert? #t))
                      (list 0 17)
                      (list #f #f)))
