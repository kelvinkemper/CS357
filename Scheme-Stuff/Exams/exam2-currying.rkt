#lang racket

(define sum-of-squares
    (lambda (a b c d)
        (+ (* a a) (* b b) (* c c) (* d d))))

;(sum-of-squares 1 2 3 4)

;((((sum-of-squares-c 1) 2) 3) 4)
;(((((curry4 sum-of-squares) 1) 2) 3) 4)

(define sum-of-squares-c
    (lambda (a)
        (lambda (b)
            (lambda (c)
                (lambda (d)
                    (+ (* a a) (* b b) (* c c) (* d d)))))))
(display "sum-of-squares-c\n")
((((sum-of-squares-c 1) 2) 3) 4)




(define curry4
 (lambda (f)
  (lambda (x1)
    (lambda (x2)
      (lambda (x3)
        (lambda (x4)
          (f x1 x2 x3 x4)))))))
(display "curry4\n")
(((((curry4 sum-of-squares) 1) 2) 3) 4)




