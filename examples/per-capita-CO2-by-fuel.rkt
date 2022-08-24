#lang at-exp complot

;; https://ourworldindata.org/emissions-by-fuel#per-capita-how-do-coal-oil-gas-and-cement-emissions-compare
;; 2nd plot down

(require gregor
         gregor/period
         sawzall
         threading
         data-frame
         syntax/parse/define)

(define data (read-data "per-capita-CO2-by-fuel.csv"))

(define-simple-macro (sum-col df name ...)
  (aggregate df [name (name) (apply + (vector->list name))] ...))

(define 2020-data
  (where data (Year) (= Year 2020)))

(define 2020-data-tidy
  (~> data
      (where (Year) (= Year 2020))
      (pivot-longer ["other-industry"
                     "flaring"
                     "cement"
                     "gas"
                     "oil"
                     "coal"]
                    #:names-to "source"
                    #:values-to "tons-co2")))

(define 2020-data-tidy/countries
  (~> 2020-data-tidy
      (where (Entity) (member Entity '("Australia"
                                       "United States"
                                       "Germany"
                                       "South Africa"
                                       "China"
                                       "United Kingdom"
                                       "World"
                                       "France"
                                       "Sweden"
                                       "Brazil"
                                       "India")))))

(current-complot-color-map 'set2)
(render (with (plot 2020-data-tidy/countries)
              (y-axis)
              (x-axis #:label "Tons of CO2")
              (title "Per capita CO2 emissions by fuel type")
              (stacked-bars #:invert? #t
                            #:x "Entity"
                            #:y "tons-co2"
                            #:group-by "source"
                            #:y-converter (λ (x) (or x 0))
                            #:labels? #f)
              (legend #:position 'right))
        #:width 1000)
