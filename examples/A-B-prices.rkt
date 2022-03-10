#lang complot

(define data
  (read-data "A-B-price-data.csv"))

;; 1: Summarize the shape of the data
(describe data)
(define canvas (plot data))

;; 2, 3: What renderer do I need...?
(bars #:x "day" #:y "A")
(points #:x "day" #:y "A")

;; 4. Points with lines together seem good.
;;    Also stylize them to see better.
(define base-plot
  (add-to canvas
          (points #:x "day" #:y "A"
                  #:color "blue"
                  #:type 'fullcircle)
          (line #:x "day" #:y "A"
                #:color "blue")))

;; 5: Add axes to make sense of the values
(define plot/rough-axes
  (add-to base-plot
          (x-axis)
          (y-axis #:min 0)))

;; 6: Improve the axes bounds, and label
(define plot/axes
  (add-to plot/rough-axes
          (x-axis #:label "day")
          (y-axis #:min 15 #:max 30
                  #:label "price ($)")))

;; 7: Add a title
(define A-price-plot
  (add-to plot/axes
          (title "A's prices are high")))

;; 8: Make a different version
;;    to compare A and B
(define A-B-price-plot
  (add-to A-price-plot
          (points #:x "day" #:y "B"
                  #:color "red"
                  #:type 'triangle)
          (line #:x "day" #:y "B"
                #:color "red")
          (title "B's prices are below A")
          (legend)))
