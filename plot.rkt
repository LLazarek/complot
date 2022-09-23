#lang at-exp racket

(provide make-plot
         plot-set
         plot-update
         (struct-out plot)
         with)

(require syntax/parse/define
         (for-syntax racket/syntax)
         "basics.rkt"
         "elements.rkt"
         "renderers.rkt"
         "error-reporting.rkt")

(struct plot complot-printable (data x-axis y-axis legend title renderers))

;; Some convenience macros
(define-simple-macro (plot-set a-plot field v)
  (struct-copy plot a-plot [field v]))
(define-simple-macro (plot-update a-plot field f v)
  #:with get-field (format-id this-syntax "plot-~a" #'field)
  (struct-copy plot a-plot [field (f v (get-field a-plot))]))

(define (make-plot data)
  (plot data #f #f #f #f empty))

(define (with a-plot . things)
  (define (add-one thing)
    (match thing
      [(? x-axis? axis)
       (plot-set a-plot x-axis axis)]
      [(? y-axis? axis)
       (plot-set a-plot y-axis axis)]
      [(? legend? legend)
       (plot-set a-plot legend legend)]
      [(title text)
       (plot-set a-plot title text)]

      [r
       #:when (is-a? r renderer<%>)
       (plot-update a-plot renderers snoc r)]

      [something-else
       (report-non-complot-argument! a-plot something-else "add-to (aka with)")]))

  (unless (plot? a-plot)
    (report-non-complot-argument! a-plot #f "add-to (aka with)"))
  (match things
    [(cons a-thing more)
     (apply with (add-one a-thing) more)]
    ['() a-plot]))

(define (snoc x l) (append l (list x)))


;; (define (with a-plot . things)
;;   (unless (is-a? a-plot plot%)
;;     (report-non-complot-argument! a-plot #f "add-to (aka with)"))
;;   (match things
;;     [(cons a-thing more)
;;      (apply with (send a-plot add-one a-thing) more)]
;;     ['() a-plot]))

;; (define (snoc x l) (append l (list x)))


;; ;; Some convenience macros
;; (define-simple-macro (plot-set a-plot field v)
;;   (begin
;;     (define copy (send a-plot shallow-copy))
;;     (set-field! field copy v)
;;     copy))

;; (define-simple-macro (plot-update a-plot field f v)
;;   (plot-set a-plot field (f v (get-field field a-plot))))

;; (define-match-expander plot
;;   (syntax-parser
;;     [(_ data-pat
;;         x-axis-pat
;;         y-axis-pat
;;         legend-pad
;;         title-pat
;;         renderers-pat)
;;      #'(? (λ (v) (is-a? v plot%))
;;           (app (λ (v) (list (get-field data v)
;;                             (get-field x-axis v)
;;                             (get-field y-axis v)
;;                             (get-field legend v)
;;                             (get-field title v)
;;                             (get-field renderers v)))
;;                (list data-pat
;;                      x-axis-pat
;;                      y-axis-pat
;;                      legend-pad
;;                      title-pat
;;                      renderers-pat)))]))

;; (define plot%
;;   (class complot-printable%
;;     (super-new)
;;     (init-field data
;;                 [x-axis #f]
;;                 [y-axis #f]
;;                 [legend #f]
;;                 [title #f]
;;                 [renderers empty])
;;     (define/public (shallow-copy)
;;       (new plot%
;;            [data data]
;;            [x-axis x-axis]
;;            [y-axis y-axis]
;;            [legend legend]
;;            [title title]
;;            [renderers renderers]))
;;     (define/public (add-one thing)
;;       (define c (shallow-copy))
;;       (add-one! c thing))
;;     (define/public (add-one! thing)
;;       (cond [(is-a? thing axis%)
;;              (if (get-field x-axis? thing)
;;                  (set! x-axis thing)
;;                  (set! y-axis thing))]
;;             [(is-a? thing legend%)
;;              (set! legend thing)]
;;             [(is-a? thing title%)
;;              (set! title thing)]
;;             [(is-a? thing renderer<%>)
;;              (set! renderers (snoc thing renderers))]
;;             [else
;;              (report-non-complot-argument! this thing "add-to (aka with)")]))))

;; (define (make-plot data)
;;   (new plot% [data data]))

;; (define (with a-plot . things)
;;   (unless (is-a? a-plot plot%)
;;     (report-non-complot-argument! a-plot #f "add-to (aka with)"))
;;   (match things
;;     [(cons a-thing more)
;;      (apply with (send a-plot add-one a-thing) more)]
;;     ['() a-plot]))

;; (define (snoc x l) (append l (list x)))
