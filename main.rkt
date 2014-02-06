#lang racket
(provide plot/linear)
(require plot)

(define (plot/linear pts-x pts-y [error null])
  (plot/gen pts-x pts-y error linear-fit))

(define (plot/gen pts-x pts-y error fit)
  (list (points (map list pts-x pts-y)
                #:x-min 0
                #:y-min 0)
        (fit pts-x pts-y)
        (error-bars (for/list ([x pts-x] [y pts-y] [δ error])
                     (list x y (* x δ))))))

(define (linear-fit pts-x pts-y)
  (define len (length pts-x))
  (define Σx (apply + pts-x))
  (define Σy (apply + pts-y))
  (define Σxy (apply + (map * pts-x pts-y)))
  (define ΣxΣy (* Σx Σy))
  (define Σx^2 (apply + (map sqr pts-x)))
  (define slope
    (/ (- (* len Σxy) ΣxΣy)
       (- (* len Σx^2) (sqr Σx))))
  (define offset
    (/ (- Σy (* slope Σx))
       len))
  (line (lambda (x) (+ (* slope x) offset))))


  
