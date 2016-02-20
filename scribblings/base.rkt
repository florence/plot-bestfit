#lang racket/base

(require
 scribble/eval
 scribble/manual

 (for-label racket bestfit plot))

(provide
 (all-from-out scribble/eval
               scribble/manual)
 (for-label (all-from-out racket bestfit plot)))
