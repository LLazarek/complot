#lang at-exp racket

(provide x-axis->plot:axis
         y-axis->plot:axis
         axis->label
         axis->ticks+transform)

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
  (define-values {the-min the-max categorical?}
    (cond [(and (axis-min an-axis)
                (axis-max an-axis))
           ;; Avoid calculating any inferred bounds if not needed
           (values (axis-min an-axis) (axis-max an-axis) #f)]
          [else
           (define-values {inferred-min inferred-max categorical?}
             (infer-bounds data renderers (x-axis? an-axis)))
           (values (or (axis-min an-axis) inferred-min)
                   (or (axis-max an-axis) inferred-max)
                   categorical?)]))
  (define extra-ticks
    (append (if (axis-ensure-min-tick? an-axis)
                (list the-min)
                empty)
            (if (axis-ensure-max-tick? an-axis)
                (list the-max)
                empty)
            (axis-minimum-ticks an-axis)))
  (define plot:ticks-fn (second (axis->ticks+transform an-axis)))
  (list the-min
        the-max
        (list (make-plot:axis
               (if (and (axis-ticks? an-axis)
                        (not categorical?))
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
                   ;; either categorical? is true, so we need the axis, but since it's categorical we leave it up to plot's internal handling above in render
                   ;; otherwise we don't need the ticks at all
                   empty))
              (if (axis-tick-lines? an-axis)
                  (if (x-axis? an-axis)
                      (plot:x-tick-lines)
                      (plot:y-tick-lines))
                  empty))))

(define (axis->ticks+transform axis)
  (match (and axis (axis-layout axis))
    [(or #f 'auto) (list (plot:plot-x-transform) (plot:plot-x-ticks))]
    ['linear (list plot:id-transform (plot:linear-ticks))]
    ['log (list plot:log-transform (plot:log-ticks))]
    [other other]))



(define (infer-bounds data renderers x? #|otherwise y|#)
  (define (real-min/maxes vs)
    (list (apply min vs) (apply max vs) #f))
  (define mins+maxes+categorical?
    (for/list ([renderer (in-list renderers)])
      (match (renderer->plot:data data renderer)
        [(list (list (? real? xs) _) ...)
         #:when x?
         (real-min/maxes xs)]
        [(list (list _ (? real? ys)) ...)
         #:when (not x?)
         (real-min/maxes ys)]
        [(list (list (? (negate real?) xs) _) ...)
         #:when x?
         (list 0 (length xs) #t)]
        [(list (list _ (list (? real? y-part-lists) ...)) ...)
         #:when (not x?)
         (define y-totals
           (for/list ([y-parts (in-list y-part-lists)])
             (apply + y-parts)))
         (real-min/maxes y-totals)]
        [(list (list _ (? (negate real?) ys)) ...)
         #:when (not x?)
         (list 0 (length ys) #t)])))
  (values (apply min (map first mins+maxes+categorical?))
          (apply max (map second mins+maxes+categorical?))
          (andmap third mins+maxes+categorical?)))
