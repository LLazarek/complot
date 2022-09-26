#lang at-exp racket

(provide check-plot:data-types!
         report-non-complot-argument!
         check-aligned!)

(require "structs.rkt"
         "util.rkt"
         data-frame
         sawzall)

(define (check-plot:data-types! a-renderer data plot:data)
  (match-define (renderer _ (converters x-conv y-conv group-conv))
    a-renderer)
  (match* {a-renderer plot:data}
    ;; The data formats that plot accepts: ok
    [{(? point-label?) (list (? real?) (? real?))}
     (void)]
    [{(or (? points?)
          (? line?))
      (list (list (? real?) (? real?)) ...)}
     (void)]
    [{(? bars?)
      (list (list _ (? real?)) ...)}
     (void)]
    [{(? stacked-bars?)
      (list (list _ (list (? real?) ...)) ...)}
     (void)]
    [{(or (? histogram?)
          (? function?)
          (? line-marker?))
      _}
     (void)]
    [{(? stacked-area?)
      (list (list (list (? real?) (? real?)) ...) ...)}
     (void)]
    ;; Others: error
    [{(? point-label?) (list x y)}
     (error 'complot
            @~a{
                Error: point-labels can only take real numbers for their x and y position
                but you made a point-label with something else:
                x = @~s[x]
                y = @~s[y]
                })]
    [{(and r
           (or (struct* points ([x-col x-col]
                                [y-col y-col]))
               (struct* line ([x-col x-col]
                              [y-col y-col]))))
      (list-no-order (and (not (list (? real?) (? real?)))
                          bad-point)
                     _ ...)}
     (error 'complot
            @~a{
                Error: points and lines can only use real number data, but I got a bit of data
                for the @(if (points? r) 'points 'line) with #:x @~s[x-col] and #:y @~s[y-col]
                which is something else (x y):
                @~s[bad-point]
                There might be a problem in the data set, with the converters (shown below),
                or you might have put a mistaken column for either #:x or #:y.
                x-converter: @x-conv
                y-converter: @y-conv
                })]
    [{(struct* bars ([x-col x-col]
                     [y-col y-col]))
      (list-no-order (list _ (and bad-y-val (not (? real?))))
                     _ ...)}
     (error 'complot
            @~a{
                Error: bars can only use real number data for its #:y values, but I got a value
                from the data for the bars with #:x @~s[x-col] and #:y @~s[y-col] for which
                the y value is something else:
                @~s[bad-y-val]
                There might be a problem in the data set, with the converters (shown below),
                or you might have put a mistaken column for #:y.
                y-converter: @y-conv
                })]
    [{(struct* stacked-bars ([x-col x-col]
                             [group-col group-col]
                             [y-col y-col]))
      (list-no-order (list x-value (and (not (list (? real?) ...)) bad-group-vals))
                     _ ...)}
     (error 'complot
            @~a{
                Error: stacked-bars can only use real number data for its #:y values, but I got
                a value from the data for the stacked-bars with
                #:x @~s[x-col] and #:y @~s[y-col] and #:group-by @~s[group-col] for which
                the y values are something else:
                x: @~s[x-value]
                y-values: @~s[bad-group-vals]

                The converters for this renderer are:
                x-converter: @x-conv
                y-converter: @y-conv
                group-converter: @y-conv
                })]
    [{(struct* stacked-area ([x-col x-col]
                             [group-col group-col]
                             [y-col y-col]))
      (list-no-order (list-no-order (and (not (list (? real?) (? real?))) bad-point)
                                    _ ...)
                     _ ...)}
     (error 'complot
            @~a{
                Error: stacked-area can only use real number data for its #:x and #:y values,
                but I got a value from the data for the stacked-area with
                #:x @~s[x-col] and #:y @~s[y-col] and #:group-by @~s[group-col] for which there
                is a point that is something else:
                @~s[bad-point]

                The converters for this renderer are:
                x-converter: @x-conv
                y-converter: @y-conv
                group-converter: @y-conv
                })]))

(define (report-non-complot-argument! a-plot a-thing name)
  (match* {a-plot a-thing}
    [{(not (? plot?)) _}
     (error 'complot
            @~a{
                Error: @name called with something that isn't a plot.
                Given: @~e[a-plot]

                Use `(plot <some-data>)` to create a plot.
                })]
    [{_ (? procedure? p)}
     (error 'complot
            @~a{
                Error: @name can't handle the procedure @~e[p]

                It looks like you are trying to use a complot thing, but you
                gave the creator of the thing instead of the thing.
                For example, you may have written something like,
                (add-to ... x-axis)
                when you should instead write,
                (add-to ... (x-axis))
                })]
    [{_ something-else}
     (error 'complot
            @~a{
                Error: @name can't handle things like @~e[something-else]
                })]))

;; (listof points-list?) renderer? -> any
;; Checks that all of the points in grouped-points are aligned: i.e. that
;; for every x-value in every points-list, there a corresponding x-value
;; in every other points-list.
;; Assumes that each points-list in `grouped-points` is sorted by first / x values.
(define (check-aligned! grouped-points a-renderer group-sequence)
  (define group-lengths (map length grouped-points))
  (unless (apply = group-lengths)
    (error 'complot
           @~a{
               @object-name[a-renderer] requires all of the groups in the dataset to be aligned
               by the #:x column, but found that the data has an unequal number of data points
               for different groups. Found: ((<group-name> <count>) ...)
               @~s[(map list group-sequence group-lengths)]
               }))
  (apply for-each
         (λ points
           (define xs (map first points))
           (unless (apply = xs)
             (error 'complot
                    @~a{
                        @object-name[a-renderer] requires all of the groups in the dataset
                        to be aligned by the #:x column, but found the following points that
                        are not aligned across groups:
                        @~s[points]
                        })))
         grouped-points))
