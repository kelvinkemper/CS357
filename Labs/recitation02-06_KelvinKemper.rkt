#lang racket

;; non tail recursive
(define interval
  (lambda (n)
    (if
      (= n 0)
      null
      (cons n (interval (- n 1))))))


;; tail recursive (doesn't work though)
(define interval2
  (lambda (n acc)
    (if 
      (= n 1) 
      acc
      (interval2 (n - 1) (cons n acc)

