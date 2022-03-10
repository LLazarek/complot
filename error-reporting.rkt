#lang at-exp racket

(provide check-plot:data-types!
         report-non-complot-argument!)

(require "structs.rkt"
         "util.rkt"
         data-frame
         sawzall)

(define (check-plot:data-types! renderer data plot:data)
  (match* {renderer plot:data}
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
    ;; histogram isn't here, it can't be wrong!
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
           (or (points _ x-col y-col _)
               (line _ x-col y-col)))
      (list-no-order (and (not (list (? real?) (? real?)))
                          bad-point)
                     _ ...)}
     (error 'complot
            @~a{
                Error: points and lines can only use real number data, but I got a bit of data
                for the @(if (points? r) 'points 'line) with #:x @~s[x-col] and #:y @~s[y-col]
                which is something else (x y):
                @~s[bad-point]
                There might be a problem in the data set, or you might have put a mistaken
                column for either #:x or #:y.
                })]
    [{(bars _ x-col y-col _)
      (list-no-order (list _ (and bad-y-val (not (? real?))))
                     _ ...)}
     (error 'complot
            @~a{
                Error: bars can only use real number data for its #:y values, but I got a value
                from the data for the bars with #:x @~s[x-col] and #:y @~s[y-col] for which
                the y value is something else:
                @~s[bad-y-val]
                There might be a problem in the data set, or you might have put a mistaken column
                for #:y.
                })]
    [{(stacked-bars _ x-col facet-col y-col _ _ _)
      (list-no-order (list x-value (and (not (list (? real?) ...)) bad-group-vals))
                     _ ...)}
     (error 'complot
            @~a{
                Error: stacked-bars can only use real number data for its #:y values, but I got
                a value from the data for the stacked-bars with
                #:x @~s[x-col] and #:y @~s[y-col] and #:facet @~s[facet-col] for which the y values
                are something else:
                x: @~s[x-value]
                y-values: @~s[bad-group-vals]
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
