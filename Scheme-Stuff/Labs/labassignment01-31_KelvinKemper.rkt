#lang racket
;; Kelvin Kemper
;; function called contains? which takes 2 arguments

;; 1. (contains s1 s2) should give #t if s1 is contained in s2
;; and #f if s1 is not contained in s2
;; (contains? '(1 2 3) '(2 3 4 5 6)) => #f
;; (contains? '(1 2 3) '(3 2 1 200)) => #t

(define contains?
  (lambda (s1 s2)
    (if 
      (null? s1) 
        #t
        (and (member? (car s1) s2) ;; is car s1 a member of s2?
            (contains? (cdr s1) s2)) ;; remaining elements of s1 in s2?
    )
  )
)


;; 2. set-equals? 
;; checks if two sets are the same
;; (set-equals '(1 2 3) '(2 3 1)) => #t
;; (set-equals '(2 3 4) '(2 3 5)) => #f

(define set-equals?
  (lambda (s1 s2)
    (if
      (null? s1)
      #t
      (contains? s1 s2))))




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


(contains? '(1 2 3) '(2 3 4 5 6))
(contains? '(1 2 3) '(3 2 1 200))

(set-equals? '(2 3 4) '(2 3 5))
(set-equals? '(1 2 3) '(2 3 1))