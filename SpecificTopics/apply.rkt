#lang racket

;; general form: (apply function argument-list)

;; Suppose cube is define:
(define cubed
    (lambda (x)
        (expt x 3)))

;; can apply it to an argument list
(apply cubed '(2))