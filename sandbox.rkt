#lang racket

(define yesser '((1 2 3) (8)))

"Scope"
(define a 100)
a
(+ 7 a)

(let
    ((a 4)
     (b 3))
    (+ a b))


(define tester
    (lambda (ls)
        (list (if (pair? (car ls))
                (tester (car ls))
                (car ls)))))

                
(iota 5)



(define dot
  (lambda v0
    (lambda v1
      (apply + (map * v0 v1)))))

;((dot '(8 9 10)) '(1 2 3))