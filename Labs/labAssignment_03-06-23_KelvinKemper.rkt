#lang racket

;; Write a tail-recursive function, remove-nth, which takes an item, a list of atoms, and 
;; an integer, n, as its argument and returns the list with the n-th occurrence of item removed.

(define remove-nth
    (lambda (item ls n)
        (if (null? ls)
            '()
            (if (pair? (car ls)) 
                    (cons (remove-nth item (car ls) n) (remove-nth item (cdr ls) n))
                    (if (and (equal? (car ls) item) (equal? n 0))
                                (cons (car ls) (remove-nth item (cdr ls) n))
                                (cons (remove-nth item (car ls) n) (remove-nth item (cdr ls) (sub1 n))))))))

(remove-nth 2 '(1 2 3 2 5) 2)





