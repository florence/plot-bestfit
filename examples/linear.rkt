#lang racket
(require plot "../main.rkt")
(parameterize ([plot-new-window? #t])
  (plot (graph/linear '(10 9 8 7 6 5 4 3 2 1)
                      '(1 2 3 4 5 6 7 8 9 10)
                      (build-list 10 (const .1)))))
