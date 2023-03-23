#lang racket

(define something 100)


;(let* ((something1 1)
 ;   (+ something something1))
  ;(+ something
   ; (+ something1 something2)))

;(+ something something1)

(define sum
  (lambda (ls)
    (if
      (null? ls)
      0
      (+ (car ls) (sum (cdr ls))))))

(define interval
  (lambda (n)
    (if
      (= n 0)
      null
      (cons n (interval (- n 1))))))



(define sum-it
  (lambda (ls acc)
    (if
      (null? ls)
      acc
      (sum-it (cdr ls) (+ acc (car ls) )))))

(define sum2 
  (lambda (ls)
    (sum-it ls 0)))

(define mylist (interval 1000000))

(time (sum mylist))
(time (sum2 mylist))