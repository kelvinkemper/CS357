#lang racket

;; currying - using a procedure wit two arguments and re-writing as a procedure of 
;; one argument whose value is a procedure of one argument.

;; hiding these details is commonly known as procedural abstraction

;; IN THIS CHAPTER
;;   - study the use of procedures as arguments to other procedures and as values of procedures

;; 7.1 map
;; let's add 1 to each item in ls

;; simple way
;; '(1 2 3 4 5)
;; '(2) '(2 3 4 5)

(define add1-to-each-item
  (lambda (ls)
    (if (null? ls)
        '()
        (cons (+ 1 (car ls)) (add1-to-each-item (cdr ls))))))
;; if we wanted to add 2?, now we have to write the function 
;; using (+ 2 (car ls)) instead, what about 3?
;; let's create a function that takes in a list and a number we want to add to it all

(define ls '(1 3 5 7 9))
 
(define map
  (lambda (proc ls)
    (if (null? ls)
        '()
        (cons (proc (car ls)) (map proc (cdr ls))))))

(map + '(5 4 3 2 1))

;; rather than using (map add1 ls) we can use
;; (map (lambda (num) (+ num 2) ls)) to add 2 to each item
;; where (lambda (num) (+ num 2) is the first procedural argument

;; 7.2 for-each
;(define for-each
;  (lambda (proc ls)
;    (if (not (null? ls))
;      (begin
;        (proc (car ls))
;        (for-each proc (cdr ls))))))

;; unrestricted lambda that is used to define a procedure that takes an arbitrary number of
;; arguments

(define add
  (letrec ((list-add
              (lambda (ls)
                (if (null? ls)
                    0
                    (+ (car ls) (list-add (cdr ls)))))))
    (lambda args
      (list-add args))))

(define add-2
  (lambda args
    (if (null? args)
        0
        (+ (car args) (apply add-2 (cdr args))))))


;; compose - takes two procedures, f and g as parameters, and returns
;; another procedure that is the composition of f and g
(define compose
  (lambda (f g)
    (lambda (x)
      (f (g x)))))

;; h(x) = sqrt(x + 1)
(define h (compose sqrt add1))
;; k(x) = sqrt(x) + 1
(define k (compose add1 sqrt))

