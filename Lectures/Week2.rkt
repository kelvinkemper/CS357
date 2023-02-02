#lang racket

(+ 1 2)


(lambda (x) x)

;; returns #<prodedure>
;; 

((lambda (x) x) 8)

((lambda x y) (+ x ()))