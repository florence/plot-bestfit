#lang typed/racket
(provide graph/linear
         graph/exponential
         graph/log
         graph/power

         linear-fit-params linear-fit
         exp-fit-params exp-fit
         log-fit-params log-fit
         power-fit-params power-fit)

(require plot/no-gui math/flonum)

;; math from http://mathworld.wolfram.com/LeastSquaresFitting.html

(define-type Flonums (Listof Nonnegative-Flonum))

(define-type Grapher
  (->* (Flonums Flonums)
       ((Option (Listof Flonum)))
       (Values renderer2d renderer2d renderer2d)))

(define-type Fitter (-> Flonums Flonums (-> Real Real)))


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


;; --------------------
;; | linear fit

(: linear-fit-params : (-> Flonums Flonums (Values Real Real)))
(define (linear-fit-params pts-x pts-y)
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
  (values offset slope))

(: linear-fit : Fitter)
(define (linear-fit pts-x pts-y)
  (define-values [a b]
    (linear-fit-params pts-x pts-y))
  (lambda ([x : Real]) (+ a (* b (fl x)))))

(: graph/linear : Grapher)
(define (graph/linear pts-x pts-y [error #f])
  (graph/gen pts-x pts-y error linear-fit))


;; --------------------
;; | exp fit

;; see http://mathworld.wolfram.com/LeastSquaresFittingExponential.html
(: exp-fit-params : (-> Flonums Flonums (Values Real Real)))
(define (exp-fit-params pts-x pts-y)
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
  (values (exp a) b))

(: exp-fit : Fitter)
(define (exp-fit pts-x pts-y)
  (define-values [A B]
    (exp-fit-params pts-x pts-y))
  (lambda ([x : Real]) (* A (exp (* B (fl x))))))

(: graph/exponential : Grapher)
(define (graph/exponential pts-x pts-y [error #f])
  (graph/gen pts-x pts-y error exp-fit))


;; --------------------
;; | log fit

;; see http://mathworld.wolfram.com/LeastSquaresFittingLogarithmic.html
(: log-fit-params : (-> Flonums Flonums (Values Real Real)))
(define (log-fit-params pts-x pts-y)
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

  (values a b))

(: log-fit : Fitter)
(define-predicate nnn? Nonnegative-Flonum)
(define (log-fit pts-x pts-y)
  (define-values [a b]
    (log-fit-params pts-x pts-y))
  (lambda ([x : Real])
    (define fx (fl x))
    (if (nnn? fx)
        (+ a (* b (log fx)))
        +nan.0)))

(: graph/log : Grapher)
(define (graph/log pts-x pts-y [error #f])
  (graph/gen pts-x pts-y error log-fit))


;; --------------------
;; | power fit

;; see http://mathworld.wolfram.com/LeastSquaresFittingPowerLaw.html
(: power-fit-params : (-> Flonums Flonums (Values Real Real)))
(define (power-fit-params pts-x pts-y)
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
  (values a b))

(: power-fit : Fitter)
(define-predicate pn? Positive-Flonum)
(define (power-fit pts-x pts-y)
  (define-values [a b]
    (power-fit-params pts-x pts-y))
  (lambda ([x : Real])
    (define fx (fl x))
    (if (pn? fx)
        (* (exp a) (expt fx b))
        +nan.0)))

(: graph/power : Grapher)
(define (graph/power pts-x pts-y [error #f])
  (graph/gen pts-x pts-y error power-fit))
