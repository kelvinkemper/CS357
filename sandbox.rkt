#lang racket


"Scope"
(define a 100)
a
(+ 7 a)

(let
    ((a 4)
     (b 3))
    (+ a b))