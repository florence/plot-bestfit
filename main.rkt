#lang typed/racket
(provide graph/linear
         graph/exponential
         graph/log
         graph/power

         linear-fit-params linear-fit
         exp-fit-params exp-fit
         log-fit-params log-fit
         power-fit-params power-fit)

(require plot/no-gui math/flonum
         (for-syntax racket/base syntax/parse))

;; math from http://mathworld.wolfram.com/LeastSquaresFitting.html

(define-type Flonums (Listof Nonnegative-Flonum))
(define-type Fit-Params (List Real Real))
(define-type Fit-Function (-> Real Real))

(define-type Grapher
  (->* (Flonums Flonums)
       ((Option (Listof Flonum)))
       (Values renderer2d renderer2d renderer2d)))

;; (define-fit [fitter
;;              fit-params
;;              grapher]
;;   body ...
;;   #:params [param-id ...]
;;   #:function function-output-expr)
;;
;; generates functions 'fit-params', 'fitter', and 'grapher'
;; which use the body expressions to generate a best fit line.
;; 'param-id's denote the parameters defined by the fitter
;; to be used in the function.
(define-syntax define-fit
  (syntax-parser
    [(_ [fitter:id
         fit-params:id
         grapher:id]
        calc-form:expr ...
        #:params [par:id ...]
        #:function output-expr)

     ; unhygienic identifier for fit-params/fit
     #:with pts-x (datum->syntax this-syntax 'pts-x)
     #:with pts-y (datum->syntax this-syntax 'pts-y)

     ; unhygienic identifier for output-expr
     #:with x (datum->syntax this-syntax 'x)

     ; list of 'Real ...' with length = # of parameters
     #:with [par-type ...] (map (λ (_) #'Real)
                                (syntax->list #'[par ...]))

     #'(begin
         (: fit-params : (-> Flonums Flonums (Values par-type ...)))
         (define (fit-params pts-x pts-y)
           calc-form ...
           (values par ...))

         (: fitter : (-> Flonums Flonums Fit-Function))
         (define (fitter pts-x pts-y)
           (define-values [par ...] (fit-params pts-x pts-y))
           (λ ([x : Real]) output-expr))

         (: grapher : Grapher)
         (define (grapher pts-x pts-y [error #f])
           (graph/gen pts-x pts-y error fitter)))]))


(: graph/gen : (-> (Listof Nonnegative-Flonum) (Listof Nonnegative-Flonum)
                   (Option (Listof Flonum))
                   (-> Flonums Flonums Fit-Function)
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

(define-fit [linear-fit
             linear-fit-params
             graph/linear]

  (define len (length pts-x))
  (define Σx (Σ pts-x))
  (define Σy (Σ pts-y))
  (define Σxy (Σ (map * pts-x pts-y)))
  (define ΣxΣy (* Σx Σy))
  (define Σx^2 (Σ (map sqr pts-x)))
  (define b
    (/ (- (* len Σxy) ΣxΣy)
       (- (* len Σx^2) (sqr Σx))))
  (define a
    (/ (- Σy (* b Σx))
       len))

  #:params [a b]
  #:function (+ a (* b (fl x))))


;; --------------------
;; | exp fit

(define-fit [exp-fit
             exp-fit-params
             graph/exponential]

  ;; see http://mathworld.wolfram.com/LeastSquaresFittingExponential.html

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
  (define A (exp a))
  (define B (/ (- (* Σy Σxylny) (* Σxy Σylny))
               ΣyΣx^2y-Σxy^2))

  #:params [A B]
  #:function (* A (exp (* B (fl x)))))


;; --------------------
;; | log fit

(define-predicate nnn? Nonnegative-Flonum)

(define-fit [log-fit
             log-fit-params
             graph/log]

  ;; see http://mathworld.wolfram.com/LeastSquaresFittingLogarithmic.html

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

  #:params [a b]
  #:function
  (let ([fx (fl x)])
    (if (nnn? fx)
        (+ a (* b (log fx)))
        +nan.0)))


;; --------------------
;; | power fit

(define-predicate pn? Positive-Flonum)

(define-fit [power-fit
             power-fit-params
             graph/power]

  ;; see http://mathworld.wolfram.com/LeastSquaresFittingPowerLaw.html

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

  #:params [a b]
  #:function
  (let ([fx (fl x)])
    (if (pn? fx)
        (* (exp a) (expt fx b))
        +nan.0)))
