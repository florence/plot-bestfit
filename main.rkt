#lang typed/racket
(provide graph/linear
         graph/exponential)
(require plot
         math/base)

;; math from http://mathworld.wolfram.com/LeastSquaresFitting.html

(define (graph/linear pts-x pts-y [error null])
  (graph/gen pts-x pts-y error linear-fit))
(define (graph/exponential pts-x pts-y [error null])
  (graph/gen pts-x pts-y error exp-fit))
(define (graph/log pts-x pts-y [error null])
  (graph/gen pts-x pts-y error log-fit))

(define (graph/gen pts-x pts-y error fit)
  (list (points (map list pts-x pts-y)
                #:x-min 0
                #:y-min 0)
        (fit pts-x pts-y)
        (error-bars (for/list ([x pts-x] [y pts-y] [δ error])
                     (list x y (* x δ))))))

(define Σ (curry apply +))

(define (linear-fit pts-x pts-y)
  (define len (length pts-x))
  (define Σx (Σ pts-x))
  (define Σy (Σ pts-y))
  (define Σxy (Σ (map * pts-x pts-y)))
  (define ΣxΣy (* Σx Σy))
  (define Σx^2 (Σ (map sqr pts-x)))
  (define slope
    (/ (- (* len Σxy) ΣxΣy)
       (- (* len Σx^2) (sqr Σx))))
  (define offset
    (/ (- Σy (* slope Σx))
       len))
  (line (lambda (x) (+ (* slope x) offset))))

;; see http://mathworld.wolfram.com/LeastSquaresFittingExponential.html
(define (exp-fit pts-x pts-y)
  (define lny (map log pts-y))

  (define Σx^2y  (Σ (map * (map sqr pts-x) pts-y)))
  (define Σylny  (Σ (map * pts-y lny)))
  (define Σxy    (Σ (map * pts-x pts-y)))
  (define Σxylny (Σ (map * pts-x pts-y lny)))
  (define Σy     (Σ pts-y))

  (define ΣyΣx^2y-Σxy^2 (- (* Σy Σx^2y) (sqr Σxy)))
  (define a (/ (- (* Σx^2y Σylny) (* Σxy Σxylny))
               ΣyΣx^2y-Σxy^2))
  (define b (/ (- (* Σy Σxylny) (* Σxy Σylny))
               ΣyΣx^2y-Σxy^2))
  (define A (expt euler.0 a))
  (line (lambda (x) (* A (expt euler.0 (* b x))))))


;; see http://mathworld.wolfram.com/LeastSquaresFittingLogarithmic.html
(define (log-fit pts-x pts-y)
  (define n (length pts-x))

  (define lnx   (map log pts-x))
  (define Σylnx (Σ (map * pts-y lnx)))
  (define Σy    (Σ pts-y))
  (define Σlnx  (Σ lnx))
  (define Σx    (Σ pts-x))

  (define b
    (/ (- (* n Σylnx) (* Σy Σlnx))
       (- (* n (sqr Σlnx)) (sqr Σlnx))))
  (define a
    (/ (- Σy (* b Σlnx))
       n))
  (line (lambda (x) (+ a (* b (log x))))))
