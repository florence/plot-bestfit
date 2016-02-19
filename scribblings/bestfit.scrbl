#lang scribble/doc
@(require (except-in "base.rkt" ->))
@(require (for-label
            (only-in typed/racket/base
              Listof Real Values ->)
            (only-in plot renderer2d)))
@title{Bestfit: Lines of Best Fit}
@defmodule[bestfit #:use-sources (bestfit)]

@author[(author+email "Spencer Florence" "spencer@florence.io")]

@(define eval
   (let ()
     (define e (make-base-eval))
     (e '(require typed/racket/base))
     (e '(require bestfit))
     e))

@(define-syntax-rule (inter . a) (interaction #:eval eval . a))

Bestfit is a library for calculating lines of best fit using
@hyperlink["http://mathworld.wolfram.com/LeastSquaresFitting.html"]{Least Squares Fitting}.

@table-of-contents[]

@deftogether[(@defproc[(graph/linear [xs (Listof Real)] [ys (Listof Real)]
                                     [errors (Listof Real) null])
                       (Values renderer2d renderer2d renderer2d)]
              @defproc[(graph/exponential [xs (Listof Real)] [ys (Listof Real)]
                                          [errors (U #f (Listof Real)) #f])
                       (Values renderer2d renderer2d renderer2d)]
              @defproc[(graph/log [xs (Listof Real)] [ys (Listof Real)]
                                  [errors (U #f (Listof Real)) #f])
                       (Values renderer2d renderer2d renderer2d)]
              @defproc[(graph/power [xs (Listof Real)] [ys (Listof Real)]
                                    [errors (U #f (Listof Real)) #f])
                       (Values renderer2d renderer2d renderer2d)])]{

Uses @racket[linear-fit], @racket[exp-fit], @racket[log-fit], @racket[power-fit], to generate three
@racket[renderer2d]s: A plot of the function of best fit, a plot of the points given by @racket[xs]
and @racket[ys], and error bars generated. The error bars are generated from @racket[error], which
is the percentage error on each y coordinate.

@inter[(define (f x) (* 3 (expt x 2)))]
@inter[(define (e y) (+ y (* y (/ (sub1 (random 2) 100)))))]
@inter[(graph/power '(0 1 2) (build-list 3 f))]
@inter[(graph/power '(0 1 2)
                     (build-list 3 (lambda (x) (e (f x))))
                     '(.02 .02 .02))]

}

@deftogether[(@defproc[(linear-fit [xs (Listof Real)] [ys (Listof Real)])
                       (-> Real Real)]
              @defproc[(exp-fit [xs (Listof Real)] [ys (Listof Real)])
                       (-> Real Real)]
              @defproc[(log-fit [xs (Listof Real)] [ys (Listof Real)])
                       (-> Real Real)]
              @defproc[(power-fit [xs (Listof Real)] [ys (Listof Real)])
                       (-> Real Real)])]{


Uses @elemref["http://mathworld.wolfram.com/LeastSquaresFitting.html"]{Least Squares Fitting} to
generate a best fit function of the given type.

@inter[(define line (linear-fit '(1 2 3) '(1 2 3)))]
@inter[(line 10)]
@inter[(line 12)]


}
