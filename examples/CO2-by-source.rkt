#lang at-exp complot

;; https://ourworldindata.org/emissions-by-fuel#coal-oil-gas-cement-where-do-co2-emissions-come-from

(require gregor
         gregor/period
         sawzall
         threading
         data-frame
         syntax/parse/define)

(define data (read-data "CO2-by-source.csv"))

(define-simple-macro (sum-col df name ...)
  (aggregate df [name (name) (apply + (vector->list name))] ...))

(define world-total-data
  (~> data
      (group-with "Year")
      (sum-col other-industry
               flaring
               cement
               gas
               oil
               coal)))

(define world-total-data-tidy
  (~> world-total-data
      (pivot-longer ["other-industry"
                     "flaring"
                     "cement"
                     "gas"
                     "oil"
                     "coal"]
                    #:names-to "source"
                    #:values-to "tons-co2")
      (create [billions-tons-co2 (tons-co2) (/ tons-co2 (expt 10 9))])))

(define (add-layout a-plot)
  (with a-plot
        (y-axis #:label "Tons of CO2 (billions)")
        (x-axis #:layout 'date)
        (title "Annual emissions by source")
        (legend)))

(render (add-layout
          (with (plot world-total-data-tidy)
                (stacked-area #:x "Year" #:y "billions-tons-co2" #:group-by "source"
                              #:x-converter (λ (n) (->posix (date n))))))
        #:width 1000)

(render (add-layout
         (apply with
                (plot world-total-data)
                (for/list ([source '("other-industry"
                                     "flaring"
                                     "cement"
                                     "gas"
                                     "oil"
                                     "coal")]
                           [i (in-naturals)])
                  (line #:x "Year" #:y source
                        #:x-converter (λ (n) (->posix (date n)))
                        #:color i))))
        #:width 1000)

(module+ racket-plot
  (require plot)
  (parameterize ([plot-x-ticks (ticks-add (date-ticks)
                                          (list (->posix (date 2020))))])
    (plot-pict (for/list ([line-source '("other-industry"
                                         "flaring"
                                         "cement"
                                         "gas"
                                         "oil"
                                         "coal")]
                          [i (in-naturals)])
                 (lines (~> world-total-data-tidy
                            (where (source) (string=? source line-source))
                            (df-select* "Year" "billions-tons-co2")
                            vector->list
                            (map (match-lambda [(vector d y) (list (->posix (date d)) y)]) _))
                        #:label line-source
                        #:color i))
               #:y-label "Tons of CO2 (billions)"
               #:x-label #f
               #:title "Annual emissions by source"
               #:width 1000)))
