#lang racket

;; RECURSION BEGINNINGS
(define myls '(1 2 3 4 56))
;; last-item returns the last top-level item in the list

(define last-item
  (lambda (ls)
    (cond
        ((null? (cdr ls)) (car ls))
        (else (last-item (cdr ls))))))

(define last-item-if
  (lambda (ls)
    (if (null? (cdr ls))
        (car ls)
        (last-item-if (cdr ls)))))

(define our-member?
  (lambda (value ls)
    (cond
      ((null? ls) #f)
      (equal? value (car ls) #t)
      (else 
        (our-member? value (cdr ls))))))
(our-member? 5 '(1 2 3 5))

(define our-member?2
  (lambda (value ls)
    (cond
      ((null? ls) #f)
      (else (or (equal? (car ls) value)
                (our-member?2 value (cdr ls)))))))
filter
(define remove-1st
  (lambda (item ls)
    (cond
      ((null? ls) '())
      ((equal? item (car ls)) (cdr ls))
      (else (cons (car ls) (remove-1st item (cdr ls)))))))