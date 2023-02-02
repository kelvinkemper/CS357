#lang racket

;; CS 357, Sporing 2023
;; Homework 1
;; Kelvin Kemper

;; Exercise 1.2

;; a. 10500900
;; b. 2.5e-6
;; c. 'big-number
;; d. 'cat
;; e. 'cheshire
;; f. 10500900
;; g. 'big-number
;; h. 'number2


;; Exercise 1.3

;; a. 4
;; b. 1/25
;; c. 2/3
;; d. 0.6666666666666667

(- 10 (- 8 (- 6 4)))
(/ 40 (* 50 20))
(/ 2 3)
(+ (* 0.1 20) (/ 4 -3))

;; Exercise 1.4

;; (-(* 4 7) (+ 13 5))
;; (* 3 (+ 4 (- -5 -3)))
;; (/ 2.5 (* 5 (/ 1 10)))
;; (* 5 (+ 255 (* 537 (+ 98.3 (- 375 (* 2.5 153))))))

;; Exercise 1.5
;; a. a + ((B + y) - a)
;; b. (a * B) + (y * B)
;; c. (a - B) / (a - Y)

;; Exercise 1.6
;; a. (cons 'one (cons 'two (cons 'three (cons 'four '()))))
;; b. (cons 'one (cons (cons 'two (cons 'three (cons 'four '()))) '()))
;; c. (cons 'one (cons (cons 'two (cons 'three '())) (cons 'four '())))
;; d. (cons (cons 'one (cons 'two '()))
;;    (cons (cons 'three (cons 'four '())) '()))    ;;easier for me to see with 2 lines
;; e. (cons (cons (cons 'one '()) '()) '())

;; Exercise 1.10
;; a. #f
;; b. #t
;; c. #f
;; d. #t

;; Exercise 1.14
;; a. #t
;; b. #f
;; c. #f
;; d. #t
;; e. #f
;; f. #t

;; Exercise 2.1
(define second 
  (lambda (ls)
    (car (cdr ls))))

;; Exercise 2.3
;; a. '(1 2)
;; b. '((a b) (e f))
(define make-list-of-one
  (lambda (item)
    (cons item '())))

(define make-list-of-two
  (lambda (item1 item2)
    (cons item1 (make-list-of-one item2))))

(define firsts-of-both
  (lambda (list-1 list-2)
    (make-list-of-two (car list-1) (car list-2))))

;; Exercise 2.4
(define juggle
  (lambda (ls)
    (list 
      (car (cdr (cdr ls))) 
      (car ls) 
      (car (cdr ls)))))

;; Exercise 2.6
;; a. #t
;; b. #t
;; c. #t
;; d. #f

;; Exercise 2.7
;; a. #t
;; b. #f
;; c. #t
;; d. #f

;; Exercise 2.10
(define last-item
  (lambda (ls)
    (if (null? (cdr ls)) ;; if cdr of list is null then take first element of list
      (car ls)
      (last-item (cdr ls))) ;; else cdr list until '()
    )
)  

(define member?
  (lambda (item ls)
    (if (null? ls) ;; if list is null? return false
      #f 
      ;;else the whichever or is true
      (or (equal? item (car ls)) ;; does x equal the first element of the ls?
          (member? item (cdr ls)))) ;; recursively check x with remaining elements of the list
    )
)

(define remove-1st
  (lambda (item ls)
    (if (null? ls) ;; if the list is null return list
    ls
    (if (eq? (car ls) item) ;; check car of list to see if that matches item
        (cdr ls)
        (cons (car ls) (remove-1st item (cdr ls)))))
    )
)

;; Exercise 2.12
;; (mystery '(1 2 3 4 5)) returns '(1 2 3 4)
;; This procedure could be called remove-last since it removes the last element of a list

;; Exercise 2.13
(define subst-1st
  (lambda (new old ls)
    (cond
      ((null? ls) '())
        (equal? old (car ls)) 
        (cons new (cdr ls))
        (cons (car ls) (subst-1st new old (cdr ls))))))
;; Exercise 2.14

;; Exercise 2.15

;; Exercise 2.16

;; Exercise 2.18


(define list1 '(1 2 3 4 5 6))