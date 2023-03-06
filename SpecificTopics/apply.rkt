#lang racket

;; general form: (apply function argument-list)

;; Suppose cube is define:
(define cubed
    (lambda (x)
        (expt x 3)))

;; can apply it to an argument list
(apply cubed '(2))

(define ls '(1 2 3 4 5))

(define length
  (lambda (lst)
    (cond 
        ((null? lst) 0)
        (else (+ 1 (length (cdr lst)))))))

(define tester
  (lambda (ls)
    (if (equal? (length ls) 2)
    (+ (car ls) (cadr ls))
    (tester (cdr ls)))))

(display "map\n")
(map - '(2 3 4))
(map + '(1 2 3) '(10 20 30))
(map * '(2 2 -1) '(0 3 4) '(5 4 2))


(display "\n")


(display "apply\n")
(apply + '(2 3 4))
; ==> 9
(define arguments '(10 50 100))
(apply + arguments)

(map odd? arguments)


(define apply-many
  (lambda args
    (if (null? args)
          (lambda (x) x)
        (compose
          (car args) (apply apply-many (cdr args))))))

((apply-many odd? even?) '3)
