#lang at-exp complot

(require gregor
         gregor/period
         sawzall
         threading
         data-frame)

(define data (read-data "energy-usage-data.csv"))

(define data/by-day
  (~> data
      (group-with "Type" "Date")
      (aggregate [Usage (Usage) (apply + (vector->list Usage))])
      (ungroup)

      (create [Date (Date) (iso8601->date Date)])))

(define (aggregate-usage-over-periods a-df date-seq)
  (for/data-frame (Date Usage)
    ([start-date (in-list date-seq)]
     [end-date (in-list (rest date-seq))])
    (define usage-in-period
      (~> a-df
          (where (Date) (and (date<=? start-date Date)
                             (date<? Date end-date)))
          (aggregate [sum (Usage) (apply + (vector->list Usage))])
          (df-select "sum")))
    (values start-date
            (match usage-in-period
              [(vector n) n]
              [else 0]))))

(define gas-periods
  (~> data/by-day
      (where (Type) (string=? Type "Gas"))
      (df-select "Date")
      vector->list))
(define data/by-gas-periods
  (~> data/by-day
      (where (Type) (string=? Type "Electricity"))
      (aggregate-usage-over-periods gas-periods)
      (create [Type (Usage) "Electricity"])
      (combine (where data/by-day (Type Date) (string=? Type "Gas")))
      (where (Date) (and (date<? Date (iso8601->date "2022-03-07"))
                         (date<? (iso8601->date "2021-03-01") Date)))))

(render (with (plot data/by-gas-periods)
              (x-axis #:layout 'date
                      #:tick-lines? #t
                      #:ensure-max-tick? #f
                      #:ensure-min-tick? #f)
              (y-axis #:label "kWh" #:min 0 #:tick-lines? #t)
              (stacked-area #:x "Date" #:y "Usage" #:group-by "Type"
                            #:x-converter ->posix
                            #:alpha 3/4)
              (legend))
        #:width 1000)

(module+ racket-plot
  (require plot)
  ;; This is kind of unfair since stacked areas aren't supported directly in plot...
  (define (get-raw-data type)
    (~> data/by-gas-periods
        (where (Type) (string=? Type type))
        (df-select* "Date" "Usage")
        vector->list
        (map (match-lambda [(vector d u) (list (->posix d) u)]) _)))
  (define raw-electricity-data (sort (get-raw-data "Electricity") < #:key first))
  (define raw-gas-data (sort (get-raw-data "Gas") < #:key first))
  (define gas-data (for/list ([raw-e-point (in-list raw-electricity-data)]
                              [raw-g-point (in-list raw-gas-data)])
                     (match* {raw-e-point raw-g-point}
                       [{(list d e)
                         (list _ g)}
                        (list d (+ e g))])))
  (parameterize ([plot-x-ticks (date-ticks)]
                 [plot-y-ticks (ticks-add (plot-y-ticks)
                                          (list 224.6))])
    (plot-pict (list (lines-interval (map (match-lambda [(list d _) (list d 0)])
                                          raw-electricity-data)
                                     raw-electricity-data
                                     #:color 1
                                     #:alpha 3/4
                                     #:label "Electricity")
                     (lines-interval raw-electricity-data
                                     gas-data
                                     #:color 2
                                     #:alpha 3/4
                                     #:label "Gas")
                     (x-tick-lines)
                     (y-tick-lines))
               #:x-label "kWH"

               #:y-min 0
               #:y-label "kWH"
               #:y-max 224.6 ;; this really does need to be repeated

               #:width 1000)))
