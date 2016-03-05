#lang typed/racket
(provide graph/linear
         graph/exponential
         graph/log
         graph/power

         linear-fit
         exp-fit
         log-fit
         power-fit)

(require plot/no-gui math/flonum)

;; math from http://mathworld.wolfram.com/LeastSquaresFitting.html

(define-type Grapher
  (->* ((Listof Nonnegative-Flonum) (Listof Nonnegative-Flonum))
       ((Option (Listof Flonum)))
       (Values renderer2d renderer2d renderer2d)))
(define-type Fitter (-> (Listof Nonnegative-Flonum) (Listof Nonnegative-Flonum)
                        (-> Real Real)))

(: graph/linear : Grapher)
(define (graph/linear pts-x pts-y [error #f])
  (graph/gen pts-x pts-y error linear-fit))
(: graph/exponential : Grapher)
(define (graph/exponential pts-x pts-y [error #f])
  (graph/gen pts-x pts-y error exp-fit))
(: graph/log : Grapher)
(define (graph/log pts-x pts-y [error #f])
  (graph/gen pts-x pts-y error log-fit))
(: graph/power : Grapher)
(define (graph/power pts-x pts-y [error #f])
  (graph/gen pts-x pts-y error power-fit))

(: graph/gen : (-> (Listof Nonnegative-Flonum) (Listof Nonnegative-Flonum)
                   (Option (Listof Flonum))
                   Fitter
                   (Values renderer2d renderer2d renderer2d)))
(define (graph/gen pts-x pts-y error fit)
  (values (points (map (inst list Nonnegative-Flonum) pts-x pts-y)
                  #:x-min (min 0 (apply min pts-x))
                  #:y-min (min 0 (apply min pts-y)))
          (function (fit pts-x pts-y))
          (error-bars (map (lambda ([x : Real] [y : Real] [δ : Real])
                             (list x y (* y δ)))
                           pts-x pts-y (or error (build-list (length pts-x) (const 0)))))))

(: Σ : (-> (Listof Flonum) Flonum))
(define (Σ l) (foldl + 0.0 l))

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
  (lambda ([x : Real]) (+ (* slope (fl x)) offset)))

;; see http://mathworld.wolfram.com/LeastSquaresFittingExponential.html
(: exp-fit : Fitter)
(define (exp-fit pts-x pts-y)

  (define lny (map log pts-y))

  (define x^2 (map sqr pts-x))
  (define Σx^2y  (Σ (map * x^2 pts-y)))
  (define Σylny  (Σ (map * pts-y lny)))
  (define Σxy    (Σ (map * pts-x pts-y)))
  (define Σxylny (Σ (map * pts-x pts-y lny)))
  (define Σy     (Σ pts-y))

  (define ΣyΣx^2y-Σxy^2 (- (* Σy Σx^2y) (sqr Σxy)))
  (define a (/ (- (* Σx^2y Σylny) (* Σxy Σxylny))
               ΣyΣx^2y-Σxy^2))
  (define b (/ (- (* Σy Σxylny) (* Σxy Σylny))
               ΣyΣx^2y-Σxy^2))
  (define A (exp a))
  (lambda ([x : Real]) (* A (exp (* b (fl x))))))

(define-predicate nnn? Nonnegative-Flonum)
;; see http://mathworld.wolfram.com/LeastSquaresFittingLogarithmic.html
(: log-fit : Fitter)
(define (log-fit pts-x pts-y)

  (: r* : (-> Flonum Flonum * Flonum))
  (define r* *)
  (define n (length pts-x))

  (define lnx   (map log pts-x))
  (define Σylnx (Σ (map r* pts-y lnx)))
  (define Σy    (Σ pts-y))
  (define Σlnx  (Σ lnx))
  (define Σx    (Σ pts-x))

  (define b
    (/ (- (* n Σylnx) (* Σy Σlnx))
       (- (* n (sqr Σlnx)) (sqr Σlnx))))
  (define a
    (/ (- Σy (* b Σlnx))
       n))
  (lambda ([x : Real])
    (define fx (fl x))
    (if (nnn? fx)
        (+ a (* b (log fx)))
        +nan.0)))

(define-predicate pn? Positive-Flonum)
;; see http://mathworld.wolfram.com/LeastSquaresFittingPowerLaw.html
(: power-fit : Fitter)
(define (power-fit pts-x pts-y)

  (define lnx (map log pts-x))
  (define lny (map log pts-y))
  (define n (length pts-x))
  (define Σlnx (Σ lnx))
  (define Σlny (Σ lny))
  (define Σlnxlny (Σ (map * lnx lny)))
  (define Σlnx^2 (Σ (map sqr lnx)))

  (define b (/ (- (* n Σlnxlny) (* Σlnx Σlny))
               (- (* n Σlnx^2) (sqr Σlnx))))

  (define a (/ (- Σlny (* b Σlnx)) n))

  (lambda ([x : Real])
    (define fx (fl x))
    (if (pn? fx)
        (* (exp a) (expt fx b))
        +nan.0)))
