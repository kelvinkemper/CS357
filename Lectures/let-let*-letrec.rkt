#lang racket

(define quadratic1
  (lambda (a b c)
    (list (/ (+ (- b) (sqrt (- (* b b) (* 4 a c))))
             (* 2 a))
          (/ (- (- b) (sqrt (- (* b b) (* 4 a c))))
             (* 2 a c)))))