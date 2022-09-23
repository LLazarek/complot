#lang at-exp racket

(provide current-complot-printer
         complot-printable%
         complot-printable)

(define current-complot-printer
  (make-parameter (λ (thing port)
                    (display @~a{#<complot @(object-name thing)>}
                             port))))

(define complot-printable%
  (class* object% (writable<%>)
    (super-new)
    (define/public (custom-write out)
      ((current-complot-printer) this out))
    (define/public (custom-display out)
      (custom-write out))))

(struct complot-printable ()
  #:methods gen:custom-write
  [(define (write-proc . args)
     ;; ... which just delegates to the current value of the printer parameter.
     (apply (current-complot-printer) args))])
