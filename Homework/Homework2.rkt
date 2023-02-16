#lang racket

;; CS 357 Spring 2023, Homework 2
;; Due Wed, Feb 15
;; Kelvin Kemper

(define make-deep
  (lambda (proc)
    (lambda (ls)
      (if (pair? ls)
          (proc (map (make-deep proc) ls))
          ls))))

;; Problem 4.4
;; (deepen-1 '(a b c d)) ==> '((a) (b) (c) (d))
;; (deepen-1 '()) ==> '()
(define deepen-1
  (lambda (ls)
   (if (null? ls)
       '()
        (cons (list (car ls)) (deepen-1 (cdr ls))))))

;; Problem 4.6
;; (insert-left-all 'z 'a '(a ((b a) ((a (c)))))) ==> '(z a ((b z a) ((z a (c)))))
;; (insert-left-all 'z 'a '()) ==> '()

;; condition:
;; check if list is null return '()
;; check if old = first element of list return '(new old (recusively rest of list)))
;; check if first elemnt of list is a pair
(define insert-left-all
  (lambda (new old ls)
    (cond
      ((null? ls) '())

      ((equal? (car ls) old) 
        (cons new (cons old (insert-left-all new old (cdr ls)))))

      ((pair? (car ls))
        (cons (insert-left-all new old (car ls))
              (insert-left-all new old (cdr ls))))

      (else
        (cons (car ls) (insert-left-all new old (cdr ls)))))))


;; Problem 4.10
;; (leftmost '((a b) (c (d e))) ==> 'a
;; (leftmost '(() a)) ==> '()
;; cond: check if list null, return '()
;;       check 1st element is pair?, return recursive call of first element
;; else: return first element of list
(define leftmost
  (lambda (ls)
    (cond
      ((null? ls) '())
      ((pair? (car ls)) (leftmost (car ls)))
      (else (car ls)))))


;; Problem 4.11
;; (rightmost '((((((b (c)))))))) ==> 'c
;; (rightmost '(a ())) ==> '()
;; conds: list null? return empty list
;;        check if 1st element is pair AND if cdr is null? then reduce down 
;;        check null of cdr, return car ls
;; else:  reduce ls
(define rightmost
  (lambda (ls)
    (cond
      ((null? ls) '())
      ((and (pair? (car ls)) (null? (cdr ls))) (rightmost (car ls)))
      ((null? (cdr ls)) (car ls))
      (else (rightmost (cdr ls))))))

;; Problem 4.18
;; (length-it '(1 2 3 4 (5 6 7 8))) ==> 5
;; (length-it '()) ==> 0
;; conds: ls is empty list return 0 or add1 and 
;; else:  + 1 then recursively call cdr of ls
(define length-it
  (lambda (ls)
    (cond
      ((null? ls) 0)
      (else (add1 (length (cdr ls)))))))

;; Problem 4.19
;; (mk-asc-list-of-ints 5) ==> '(1 2 3 4 5)
;; (mk-desc-list-of-ints 5) ==> '(5 4 3 2 1)
;; not sure how to use mk-asc-list-of-ints first but it uses 
;; mk-desc-list-of-ints as a helper but reversing it
(define mk-asc-list-of-ints
  (lambda (n) 
     (reverse (mk-desc-list-of-ints n))))


(define mk-desc-list-of-ints
  (lambda (n)
    (if (= n 0)
      '()
      (cons n (mk-desc-list-of-ints (- n 1))))))

;; Problem 4.20
;; (occurs 'a '(a b a c a d)) ==> 3
;; (occurs 'a '(b c a (b a) c a)) ==> 2
(define occurs
  (lambda (n ls)
      (cond
        ((null? ls) 0)
        ((equal? n (car ls))
            (add1 (occurs n (cdr ls))))
        (else 
            (occurs n (cdr ls))))))

(define occurs-it
  (lambda (n ls)
    (occurs-it-helper n 0 ls)))

(define occurs-it-helper 
  (lambda (n acc ls)
    (cond
      ((null? ls) acc)
      ((equal? n (car ls)) (occurs-it-helper n (+ acc 1) (cdr ls)))
      (else (occurs-it-helper n  acc (cdr ls))))))

;; Infix calculator for + - * /
;; (calculator '(1 + (2 * ((3 + 4) - 5)))) ==> 5
;; (calculator '(5 / (5 / ((5 / 5) / 5)))) ==> 1/5
(define calculator
  (make-deep
   (lambda (ls)
     (let ((op (cadr ls)))
       ((cond ((eq? op '+) +)
              ((eq? op '-) -)
              ((eq? op '*) *)
              (else /))
        (car ls)
	(caddr ls))))))

;; Infix-to-prefix expression converter
;; (infix->prefix '(1 + (2 * ((3 + 4) - 5)))) ==> '(+ 1 (* 2 (- (+ 3 4) 5)))
;; (infix->prefix '(5 / (5 / ((5 / 5) / 5)))) ==> '(/ 5 (/ 5 (/ (/ 5 5) 5)))
(define infix->prefix 0)

;; Iota-iota function using letrec and tail recursion
;; (iota-iota 1) ==> '((1 . 1))
;; (iota-iota 2) ==> '((1 . 1) (1 . 2) (2 . 1) (2 . 2))
(define iota-iota 0)

;; Convert digits to number tail-recursively
;; (digits->number '(7 6 1 5)) ==> 7615
;; (digits->number '(0)) ==> 0
(define digits->number 0)

;; Cond expression to nested if expressions
;; (cond->if ’(cond ((> x y) (- x y)) ((< x y) (- y x)) (else 0))) ==> (if (> x y) (- x y) (if (< x y) (- y x) 0))
(define cond->if 0)

;; Cosine function using letrec for helper functions
;; DO NOT use/define fact or expt!
;; (cos 0) ==> something that is approximately 1
;; (cos 1) ==> something that is approximately 0.5403
;; (cos 1.57079) ==> something that is approximately 1
(define cos 0)


;(deepen-1 '(a b c d))
;(deepen-1 '((a b) (c (d e)) f))
;(deepen-1 '())

;(insert-left-all 'z 'a '(a ((b a) ((a (c))))))
; Value: (z a ((b z a) ((z a (c)))))

;(insert-left-all 'z 'a '(((a))))
; Value: (((z a)))

;(insert-left-all 'z 'a '())
; Value: ()

(leftmost '((a b) (c (d e))))
; Value: a
(leftmost '((((c ((e f) g) h)))))
; Value: c
(leftmost '(() a))
; Value: ()

(rightmost '((a b) (d (c d (f (g h) i) m n) u) v))
; Value: v
(rightmost '((((((b (c))))))))
; Value: c
(rightmost '(a ()))
; Value: ()

(length-it '(1 2 3 4 (5 6 7 8)))
; 5
(length-it '())
; 0

(mk-asc-list-of-ints 5)
;; '(1 2 3 4 5)
(mk-desc-list-of-ints 5) 
;; '(5 4 3 2 1)

(occurs 'a '(a b a c a d)) 
;; 3
(occurs 'a '(b c a (b a) c a)) 
;; 2

(occurs-it 'a '(a b a c a d)) 
;; 3
(occurs-it 'a '(b c a (b a) c a)) 
;; 2

(calculator '(1 + (2 * ((3 + 4) - 5))))
;;5
(calculator '(5 / (5 / ((5 / 5) / 5))))
;;1/5