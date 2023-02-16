#lang racket

(define lis '(1 2 3 4 5))
;; flat recursion uses top-level items of a list by applying the procedure to the cdr of the list

;; append takes a list as the first argument and list/atom as second
;; append using recursion on the first list ls1, cdr on ls1 ultimately produces the base case 
;; this is where ls1 is empty. At base case, when ls1 is empty, ls2 is returned.
(append '(a b c) '(c d)) ;; -> (a b c c d)
;; (a b c) (c d)
;; cons a AND (b c c d) => (a b c c d)
;; () (a b c c d) => returns ls2
(define our-append
  (lambda (ls1 ls2)
    (if (null? ls1)
        ls2
        (cons (car ls1) (append (cdr ls1) ls2)))))


(define our-reverse
  (lambda (ls)
    (if (null? ls)
      '()
      (our-append (our-reverse (cdr ls)) (list (car ls))))))
