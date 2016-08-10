#lang scribble/doc
@(require (except-in "base.rkt" ->)
          plot/utils plot/no-gui)
@(require (for-label
           (only-in math/flonum fl)
           (only-in typed/racket/base
                    Listof Nonnegative-Flonum Values -> cast)
           (only-in plot/utils renderer2d?)))
@title{Bestfit: Lines of Best Fit}
@defmodule[bestfit #:use-sources (bestfit)]

@author[(author+email "Spencer Florence" "spencer@florence.io")]

@(define eval
   (let ()
     (define e (make-base-eval))
     (e '(require racket))
     (e '(require bestfit))
     (e '(require plot/pict))
     (e '(require math/flonum))
     e))

@(define-syntax-rule (inter . a) (interaction #:eval eval . a))

Bestfit is a library for calculating lines of best fit using
@hyperlink["http://mathworld.wolfram.com/LeastSquaresFitting.html"]{Least Squares Fitting}.

@table-of-contents[]

@deftogether[(@defproc[(graph/linear [xs (Listof Nonnegative-Flonum)]
                                     [ys (Listof Nonnegative-Flonum)]
                                     [errors (U #f (Listof Flonum)) #f])
                       (Values renderer2d? renderer2d? renderer2d?)]
              @defproc[(graph/exponential [xs (Listof Nonnegative-Flonum)]
                                          [ys (Listof Nonnegative-Flonum)]
                                          [errors (U #f (Listof Flonum)) #f])
                       (Values renderer2d? renderer2d? renderer2d?)]
              @defproc[(graph/log [xs (Listof Nonnegative-Flonum)]
                                  [ys (Listof Nonnegative-Flonum)]
                                  [errors (U #f (Listof Flonum)) #f])
                       (Values renderer2d? renderer2d? renderer2d?)]
              @defproc[(graph/power [xs (Listof Nonnegative-Flonum)]
                                    [ys (Listof Nonnegative-Flonum)]
                                    [errors (U #f (Listof Flonum)) #f])
                       (Values renderer2d? renderer2d? renderer2d?)])]{

Uses @racket[linear-fit], @racket[exp-fit], @racket[log-fit], @racket[power-fit], to generate three
@racket[renderer2d?]s: A plot of the points given by @racket[xs]
and @racket[ys], a plot of the function of best fit, and error bars generated. The error bars are generated from @racket[error], which
is the percentage error on each y coordinate.

@inter[;(: 3x^2 : Nonnegative-Flonum -> Nonnegative-Flonum)
       (define (3x^2 x) (* 3.0 (expt x 2.0)))
       ;(: apply-error : Nonnegative-Flonum -> Nonnegative-Flonum)
       (define (add-error y) (+ y (* y (/ (- (random 4) 2) 10.0))))
       (define exact (function 3x^2 #:label "exact" #:color "blue"))
       (define-values (pts fit _)
         (graph/power (build-list 10 (compose fl add1))
                      (build-list 10 (compose 3x^2 fl add1))))
       (plot (list exact fit pts))
       (define-values (pts fit err)
         (graph/power (build-list 10 (compose fl add1))
                      (build-list 10 (compose add-error 3x^2 fl add1))
                      (build-list 10 (const 0.2))))
       (plot (list exact fit pts err))]

}

@deftogether[(@defproc[(linear-fit [xs (Listof Nonnegative-Flonum)]
                                   [ys (Listof Nonnegative-Flonum)])
                       (-> Nonnegative-Flonum Real)]
              @defproc[(exp-fit [xs (Listof Nonnegative-Flonum)]
                                [ys (Listof Nonnegative-Flonum)])
                      (-> Nonnegative-Flonum Real)]
              @defproc[(log-fit [xs (Listof Nonnegative-Flonum)]
                                [ys (Listof Nonnegative-Flonum)])
                      (-> Nonnegative-Flonum Real)]
              @defproc[(power-fit [xs (Listof Nonnegative-Flonum)]
                                  [ys (Listof Nonnegative-Flonum)])
                       (-> Nonnegative-Flonum Real)])]{


Uses @hyperlink["http://mathworld.wolfram.com/LeastSquaresFitting.html"]{Least Squares Fitting} to
generate a best fit function of the given type.

@inter[(define line (linear-fit '(1.0 2.0 3.0) '(1.0 2.0 3.0)))
       (line 10.0)
       (line 12.0)]


}
