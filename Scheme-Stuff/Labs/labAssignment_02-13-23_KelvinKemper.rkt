#lang racket

;; tail recursive functions

;; interfaces
(define zilch '())

(define zilch? null?)

(define succ (lambda (n) (cons 'a n)))

(define pred cdr)

;; converst a natural number from out interface
;; into an ordinary scheme integer
(define nat->number 
  (lambda (n)
    (if (zilch? n)
        0
        (+ 1 (nat->number (pred n))))))

 (define number->nat
   (lambda (n)
     (if (= n 0)
        zilch
        (succ (number->nat (sub1 n))))))     

(define subtract
  (lambda (m n)
    (cond
      ((zilch? n) m)
      ((zilch? m) zilch)
      (else (subtract (pred m) (pred n))))))



;; 1. (divides? d n)
;; returns true if n is divisible by d, and false #f otherwise
;; (divides? 3 9) ==> #t
;; (divides? 3 10) ==> #f
;; (divides? 2 1000) ==> #t
;; (divides? 2 1001) ==> #f
;; (divides? 10 150) ==> #t
;; don't worry about d being zero, that is undefined behavior

(define divides?
  (lambda (d n)
    (cond
     ((zilch? n) #t)
     ((< n 0) #f)
     (else (divides? d (subtract (n d)))))))



;; 2. (count-powers-of-two n)
;;    count the number of perfectd powers of two less than or equal to n
;; (count-powers-of-two (number->nat 10)) ==> 4
;; (count-powers-of-two (number->nat 16)) ==> 5
;; (count-powers-of-two (number->nat 1)) ==> 1

(define count-powers-of-two 0)
