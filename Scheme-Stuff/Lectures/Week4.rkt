#lang racket
;; What is the interface for stack?
;; pop, push, (sometimes top)
;; 

;; abstract interfaces - ****
;; abstract datatypes advantages instead of using concrete datatypes
;; advantages: information hiding, can swap out solely the abstract datatype without
;; affecting the whole thing

;; successor, predecessor, 

(define zilch '())

(define zilch? null?)

(define succ (lambda (x) (cons '() x)))

(define pred cdr)

(define number->nat 
  (lambda (k)
    (if (zero? k)
        zilch
        (succ (number->nat (sub1 k))))))


(define nat->number
  (lambda (x)
    (if (zilch? x)
        0
        (add1 (nat->number (pred x))))))

(define smaller?
  (lambda (x y)
    (cond ((zilch? x) (not (zilch? y)))
          ((zilch? y) #f)
          (else
            (smaller? (pred x) (pred y))))))

;; implementing subtraction
;; x - 0 = x
;; x - y = (x - 1) - (y - 1)
(define minus
  (lambda (x y)
    x
    (minus (pred x) (pred y))))

;; x + 0 = x
;; x + y = (x + 1) + (y - 1)
(define plus
  (lambda (x y)
    (if (zilch? y)
        x
        (plus (succ x) (pred y)))))

;; x * 0 = 0
;; x * 1 = x
;; x * y = x * (y-1) + x
(define times
  (lambda (x y)
    (if (zilch? y)
      zilch
      (plus (times x (pred y)) x))))


;; x ^ 0 = 1
;; x ^ 1 = x
(define power
  (lambda (x y)
    (if (zilch? y)
      (succ zilch)
      (times (power x (pred y)) x))))


;; primitive recursive function that is extremely hard for computers
(define ackerman
  (lambda (n)
    (if (zilch? n)
        plus
        (lambda (x y)
          (cond ((zilch? y) (pred n))
                ((zilch? (pred y)) x)
                (else
                  ((ackerman (pred n)) ((ackerman n) x (pred y)) x)))))))


;; vars 