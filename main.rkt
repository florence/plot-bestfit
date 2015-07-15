#lang typed/racket
(provide graph/linear
         graph/exponential
         graph/log

         linear-fit
         exp-fit
         log-fit)

(require plot
         math/base)

;; math from http://mathworld.wolfram.com/LeastSquaresFitting.html

(define-type Grapher
  (->* ((Listof Real) (Listof Real))
       ((Listof Real))
       (Values renderer2d renderer2d renderer2d)))
(define-type Fitter (-> (Listof Real) (Listof Real) (-> Real Real)))

(: graph/linear : Grapher)
(define (graph/linear pts-x pts-y [error null])
  (graph/gen pts-x pts-y error linear-fit))
(: graph/exponential : Grapher)
(define (graph/exponential pts-x pts-y [error null])
  (graph/gen pts-x pts-y error exp-fit))
(: graph/log : Grapher)
(define (graph/log pts-x pts-y [error null])
  (graph/gen pts-x pts-y error log-fit))

(: graph/gen : (-> (Listof Real) (Listof Real) (Listof Real) Fitter
                   (Values renderer2d renderer2d renderer2d)))
(define (graph/gen pts-x pts-y error fit)
  (values (points (map (inst list Real) pts-x pts-y)
                  #:x-min 0
                  #:y-min 0)
          (function (fit pts-x pts-y))
          (error-bars (map (lambda ([x : Real] [y : Real] [δ : Real])
                             (list x y (* x δ)))
                           pts-x pts-y error))))

(: Σ : (-> (Listof Real) Real))
(define (Σ l) (apply + l))

(: linear-fit : Fitter)
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
  (lambda ([x : Real]) (+ (* slope x) offset)))

;; see http://mathworld.wolfram.com/LeastSquaresFittingExponential.html
(: exp-fit : Fitter)
(define (exp-fit pts-x p-y)

  (define-values (pts-y Δy) (shift-pos p-y))

  (define lny (map log pts-y))

  (: r* : (-> Real * Real))
  (define r* *)

  (define Σx^2y  (Σ (map r* (map sqr pts-x) pts-y)))
  (define Σylny  (Σ (map r* pts-y lny)))
  (define Σxy    (Σ (map r* pts-x pts-y)))
  (define Σxylny (Σ (map r* pts-x pts-y lny)))
  (define Σy     (Σ pts-y))

  (define ΣyΣx^2y-Σxy^2 (- (* Σy Σx^2y) (sqr Σxy)))
  (define a (/ (- (* Σx^2y Σylny) (* Σxy Σxylny))
               ΣyΣx^2y-Σxy^2))
  (define b (/ (- (* Σy Σxylny) (* Σxy Σylny))
               ΣyΣx^2y-Σxy^2))
  (define A (expt euler.0 a))
  (lambda ([x : Real]) (- (* A (expt euler.0 (* b x))) Δy)))


;; see http://mathworld.wolfram.com/LeastSquaresFittingLogarithmic.html
(: log-fit : Fitter)
(define (log-fit p-x pts-y)

  (define-values (pts-x Δx) (shift-pos p-x))

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
  (lambda ([x : Real]) (+ a (* b (log (cast (+ x Δx) Nonnegative-Real))))))


(: shift-pos : (-> (Listof Real) (Values (Listof Nonnegative-Real) Nonnegative-Real)))
(define (shift-pos pts)
  (: low : Real)
  (define low (apply min pts))
  (: δ : Nonnegative-Real)
  (define δ (abs (- low 0)))
  (define-values (pt diff)
    (if (> low 0)
        (values pts 0)
        (values (map (lambda ([x : Real]) (+ x δ)) pts)
                δ)))
  (values (cast pts (Listof Nonnegative-Real)) diff))
