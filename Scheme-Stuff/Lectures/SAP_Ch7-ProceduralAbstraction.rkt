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

(define list (lambda args args))
;(list '(1 2 3))

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

;(map + '(5 4 3 2 1))

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



;;;;;;;;;;;;;;;;;; 7.3 CURRYING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define add5
  (lambda (n)
    (+ 5 n)))

(define curried+
  (lambda (m)
    (lambda (n)
      (+ m n))))

;((curried+ 5) 7) ==> 12
(define add8 (curried+ 8))
;(add8 7) => 15

;;;;;;;;;;;;;;;;;; 7.4 Procedural Abstraction of Flat Recursion ;;;;;;;;;;;;;;;;;;
(define flat-recur
  (lambda (seed list-proc)
    (letrec
      ((helper
          (lambda (ls)
            (if (null? ls)
                seed
                (list-proc (car ls) (helper (cdr ls)))))))
      helper))) 

(define member?-c
  (lambda (item)
    (flat-recur #f (lambda (x y) (or (equal? x item) y)))))

;((member?-c 5) '(1 2 5))

(define apply-to-all
  (lambda (proc)
    (flat-recur '() (lambda (x y) (cons (proc x) y)))))
;((apply-to-all even?) '(2 4 6))

(define sum (flat-recur 0 +))
;(sum '(1 2 2))

(define product (flat-recur 1 *))
 

(define book-flat-recur
  (lambda (seed list-proc)
    (letrec
        ((helper
          (lambda (ls)
            (if (null? ls)
                seed
                (list-proc (car ls) (helper (cdr ls)))))))
        helper)))

;((book-flat-recur 5 +) '(1 2 3))

(define mult-by-scalar
  (lambda (n ls)
    (if (null? ls) 
        '()
        (cons (* n (car ls)) (mult-by-scalar n (cdr ls))))))
;; seed = '(), (lambda (x y) (cons (* x) y))
;(mult-by-scalar 5 '())

(define mult-by-scalar2 
  (lambda (n)
    (book-flat-recur '() (lambda (x y) (cons (* n x) y)))))

;((mult-by-scalar2 5) '(3 2 4))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 7.5 Procedural Abstraction of Deep Recursion ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define deep-recur
  (lambda (seed item-proc list-proc)
    (letrec
      ((helper
          (lambda (ls)
            (if (null? ls)
                seed
                (let ((a (car ls)))
                  (if (or (pair? a) (null? a))
                      (list-proc (helper a) (helper (cdr ls)))
                      (item-proc a (helper (cdr ls)))))))))
        helper)))

(define reverse-all
  (lambda (ls)
    (cond
      ((null? ls) '())
      ((pair? (car ls))
        (append (reverse-all (cdr ls))
                (list (reverse-all (car ls)))))
      (else
        (append (reverse-all (cdr ls))
                (list (car ls)))))))

(define reverse-all-deep
  (lambda (ls)
    (if (null? ls)
        '()
        (append (reverse-all-deep (cdr ls))
                (list (if (pair? (car ls))
                          (reverse-all-deep (car ls))
                          (car ls)))))))

(define reverse-all-myform
    (letrec
        ((helper
          (lambda (ls)
            (if (null? ls)
                '()
                (let ((a (car ls)))
                  (if (or (pair? a) (null? a))
                    (append (helper (cdr ls)) (list a))
                    (append (helper (cdr ls) (list (helper a))))))))))
        helper))
;(reverse-all-myform '(1 2 3 4))


; (reverse-all-deep '(1 3 (81 4) 5 (32) 3 ))                         

;(reverse-all-deep-recur '(1 2 3 4))
;;; now using deep-recur rewrite using reverse-all
; seed = '(), item-proc = ?  , list-proc = ?

;(define reverse-all-d (deep-recur '() list append))

;(reverse-all-d '((1 2 3) (4 5) ((6 7 8) (9 10 11))))
;;   ==> '(((11 10 9) (8 7 6)) (5 4) (3 2 1))

(define sum-all
  (letrec ((helper
              (lambda (ls)
                (if (null? ls)
                    0  ;; seed
                    (let ((a (car ls)))
                      (if (or (pair? a) (null? a))
                          (+ (helper a) (helper (cdr ls)))  ;; list-proc
                          (+ a (helper (cdr ls)))))))))     ;; item-proc
            helper))

(define sum-all1 (deep-recur 0 + +))
;(sum-all1 '(1 2 3 4 5))

(define filter-in-all-c
  (lambda (pred)
    (letrec
      ((helper
            (lambda (ls)
              (if (null? ls)
                  '() ;; seed
                  (let ((a (car ls)))
                    (if (or (pair? a) (null? a))
                          (cons (helper a) (helper (cdr ls))) ;; list-proc
                          (if (pred a) ;; item proc        if (pred x)
                              (cons a (helper (cdr ls))) ;;(cons x y)  where y = (helper (cdr ls))
                              (helper (cdr ls)))))))))   ;; else y
      helper)))

;;((filter-in-all-c even?) '(2 4 1 2 5 6 8))

(define filter-in-all-c-d
  (lambda (pred)
    (deep-recur
      '()
      (lambda (x y)
        (if (pred x)
            (cons x y)
            y))
      cons)))

;((filter-in-all-c-d odd?) '(1 3 4 5 6 8 9))

;; 7.26
;(define remove-all-c
;  (lambda (item)
;    (letrec
;        ((helper
;            (lambda (ls)
;              (if (null? ls)
;                  '()
;                  (let ((a (car ls)))
;                    (if (or (pair? a) (null? a))
;                        (cons (helper a) (helper (cdr ls)))
;                        (if ())))))))