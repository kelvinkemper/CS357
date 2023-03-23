#lang racket

;; general form: (lambda var expr1 expr2 ...)

;; add function with variadic lambda
(define add
    (letrec ((list-add  ;; list-add temp function
                (lambda (ls)  ;; takes in a list as its argument
                    (if (null? ls)  ;; if list is null return 0 else
                        0
                        (+ (car ls) (list-add (cdr ls))))))) ;; add first of list then recurse on cdr of list
        (lambda args  ;; do this for all arguments 
            (list-add args))))  ;; arguments are used in list-add

(add 1 2 3 4 5)


(define list (lambda args args))
(define writeln
    (lambda args
        (for-each display args) 
        (newline)))

(define add-apply
    (lambda args
        (if (null? args)
            0
            (+ (car args) (apply add-apply (cdr args))))))

(add-apply 1 2 3 4 5)