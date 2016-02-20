#lang setup/infotab

(define version "0.2")
(define collection 'use-pkg-name)
(define name "bestfit")
(define deps '("base" "typed-racket-lib" "plot-lib" "plot-gui-lib"))
(define build-deps '("racket-doc" "typed-racket-doc" "scribble-lib"))

(define scribblings '(("scribblings/bestfit.scrbl" ())))
