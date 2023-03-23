#lang racket

;; '() <-> 0
;; '(()) <-> 1
;; '((())) <-> 2
;; ...

;; '() <--> 0
;; '(a) <--> 1
;; '(a a) <--> 2
;; '(a a a) <--> 3
;; ...

;; representation of natural numbers

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


;; (add 5 3)
;; (add 6 2)
;; (add 7 1)
;; (add 8 0)
(define add
  (lambda (m n)
    (if (zilch? n) 
        m
        (add (succ m) (pred n)))))

;; not tail recursive
(define mult-bad
  (lambda (m n)
    (if (zilch? m) 
        zilch
        (add n (mult-bad (pred m) n)))))

;; (mult-tail 4 5 0)
;; (mult-tail 3 5 5)
;; (mult-tail 2 5 10)
;; (mult-tail 1 5 15)
;; (mult-tail 0 5 20)
;; returns acc=20 since m is zilch
;;(define mult-tail
 ;; (lambda (m n acc)
 ;;   (if (zilch? m)
 ;;       acc
  ;;      (mult-tail (pred m) n (add acc n)))))

(define milt
  (lambda (m n)
    (letrec
      ((mult-tail (lambda (m n acc)
        (if (zilch? m)
            acc
            (mult-tail (pred m) n (add n acc))))))
      (mult-tail m n zilch))))