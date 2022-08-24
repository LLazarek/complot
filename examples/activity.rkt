#lang complot

;; step 1
(define data (read-data "activity-data.csv"))
(define our-plot (plot data))

;; step 2
(define day-histogram
  (add-to our-plot
          (histogram #:x "day")))
(define day-histogram/axes
  (add-to day-histogram
          (x-axis)
          (y-axis)))
(define A-price-histogram/axes
  (add-to our-plot
          (histogram #:x "price of A")
          (x-axis)
          (y-axis)))
(define B-price-histogram/axes
  (add-to our-plot
          (histogram #:x "price of B")
          (x-axis)
          (y-axis)))
;; step 3
(define A-price-line-plot
  (add-to our-plot
          (line #:x "day"
                #:y "price of A")))
(define A-price-line-plot/points
  (add-to A-price-line-plot
          (points #:x "day"
                  #:y "price of A")))
;; step 4
(define clean-data
  (read-data "activity-data-clean.csv"))
(define our-plot/clean (plot clean-data))
(define clean-day-histogram/axes
  (add-to our-plot/clean
          (histogram #:x "day")
          (x-axis)
          (y-axis)))
(define clean-A-price-histogram/axes
  (add-to our-plot/clean
          (histogram #:x "price of A")
          (x-axis)
          (y-axis)))
(define clean-B-price-histogram/axes
  (add-to our-plot/clean
          (histogram #:x "price of B")
          (x-axis)
          (y-axis)))
(define clean-A-price-line-plot/points
  (add-to our-plot/clean
          (line #:x "day" #:y "price of A")
          (points #:x "day"
                  #:y "price of A")))
;; challenge
(define A-B-price-plot
  (add-to our-plot/clean
          (line #:x "day"
                #:y "price of A")
          (points #:x "day"
                  #:y "price of A")

          (line #:x "day"
                #:y "price of B"
                #:color "blue")
          (points #:x "day"
                  #:y "price of B"
                  #:color "blue")

          (x-axis)
          (y-axis)

          (legend)))
