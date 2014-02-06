#lang racket
(require plot "../main.rkt")
(parameterize ([plot-new-window? #t])
  (plot (graph/exponential '(1 2 3 4 5) '(1 2 3 4 5))))
