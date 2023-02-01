#lang racket

(define x 5)

;; member?, length, odds, even, merge, delete

;; Represent sets as lists with non-repeated elements

;; Rewrite member? function
(define member?
  (lambda (x ls)
    (if 
      (null? ls) ;; if list is null? return false
      #f 
      ;;else the whichever or is true
      (or (equal? x (car ls)) ;; does x equal the first element of the ls?
          (member? x (cdr ls)))) ;; recursively check x with remaining elements of the list
    )
)
;; (set-insert 5 '(1 2 3)) => '(1 2 3 5)
;; (set-insert 5 '(1 2 3 4 5)) => '(1 2 3 4 5)
(define set-insert
    (lambda (x ls)
        (if (member? x ls) ls (cons x ls))
    )
)

;; (union '(1 2 3) '(2 3 4)) => '(1 2 3 4) / '(2 3 4 1) / '(3 2 4 1)

(define union
    (lambda (s1 s2)
      (if
        (null? s1)
        s2
        (union (cdr s1) (set-insert (car s1) s2)))

    )
)

;; (union '(27 28 29) '(2 3 4))
;; (union '(28 29) (set-insert 29 '(2 3 4)))
;; (union '(28 29) '(29 2 3 4))
;; (union '(29) (set-insert 28 '(29 2 3 4 )))