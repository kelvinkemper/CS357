#lang racket

;; CS 357 Homework #3, Spring 2023
;; Kelvin Kemper

;; Exercise 7.2 done
(define compose3
  (lambda (f g h)
    (lambda (x)
      (f (g (h x))))))

;((compose3 add1 sub1 add1) 5)
;; 6
;((compose3 (lambda (x) (* x x)) (lambda (x) (* x x)) add1) 3)
;; 256      

;; Exercise 7.3 done
(define compose-many
  (lambda args
    (if (null? args)
          (lambda (x) x)
        (compose
          (car args) (apply compose-many (cdr args))))))

;;((compose-many add1 add1 add1 add1 add1) 5)
;; 10
;;((compose-many) 'batman)
;; 'batman

;; Exercise 7.6 done
;; use plus procedure
(define map-first-two
  (lambda (proc ls)
    (if (null? (cdr ls))
        '()
        (cons (proc (car ls) (car (cdr ls))) (map-first-two proc (cdr ls))))))

;;(map-first-two + '(5 4 3 2 1))
;;'(9 7 5 3)
;;(map-first-two cons '(1 (2 3) (4 5) (6 7 8)))
;'((1 2 3) ((2 3) 4 5) ((4 5) 6 7 8))

;; Exercise 7.7 done
(define length1
  (lambda (lst)
    (cond 
        ((null? lst) 0)
        (else (add1 (length1 (cdr lst)))))))

(define reduce
  (lambda (proc ls)
    (if (equal? (length1 ls) 2)
      (proc (car ls) (cadr ls))
    (reduce proc (cons (proc (car ls) (cadr ls)) (cddr ls))))))

;;(reduce + '(5 -1 3 7 2 3 5)) 
;; 24
;;(reduce * '(534 9549 201 59 11238 0 4848))
;; 0

;; Exercise 7.8 done
(define andmap
  (lambda (pred ls)
    (if (null? ls)
       #t
    (and (pred (car ls)) (andmap pred (cdr ls))))))

;(andmap positive? '(4 5 1 200 0 7 8))
;#f
;(andmap (lambda (ls) (not (null? ls))) '((1 2 3) (4 5) (6 7) (8) (9 10)))
;#t

;; Exercise 7.12 done
(define curried*
 (lambda (n)
   (lambda (m)
     (* n m))))

(define times10 (curried* 10))

;((curried* 11) 12)
; 132
;(map (curried* -2) '(1 2 4 8 16 32))
; '(-2 -4 -8 -16 -32 -64)

;; Exercise 7.18 done
(define between?
  (lambda (x y z)
    (cond
      ((and (< x y) (< y z)) #t)
      (else 
        #f))))

(define between?-c
  (lambda (x)
    (lambda (y)
      (lambda (z)
        (and (< x y) (< y z))))))

;;(between? 4 5 6)
; ==> #t
;;(map (lambda (f) (f 10)) (map (between?-c 3) '(1 2 3 4 8 9 10 11)))
; ==> '(#f #f #f #t #t #t #f #f)

;; Exercise 7.22 done
(define book-flat-recur
  (lambda (seed list-proc)
    (letrec
        ((helper
          (lambda (ls)
            (if (null? ls)
                seed
                (list-proc (car ls) (helper (cdr ls)))))))
      helper)))

(define mult-by-scalar 
  (lambda (c)
    (book-flat-recur '() (lambda (x y) (cons (* c x) y)))))

;((mult-by-scalar 2) '(5 6 7 8))
;; ==> '(10 12 14 16)
;(map (mult-by-scalar -1) '(() (1) (1 -1) (1 -1 1 -1)))
;; ==> '(() (-1) (-1 1) (-1 1 -1 1))

;; Exercise 7.30 NOT DONE
(display "Exercise 7.30 not done\n")
(define book-deep-recur
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
        helper
    )
  )
)

(define reverse-all-deep
  (lambda (ls)
    (if (null? ls)
        '()
        (append (reverse-all-deep (cdr ls))
                (list (if (pair? (car ls))
                          (reverse-all-deep (car ls))
                          (car ls)))))))

;(define reverse-all
;  (lambda (ls)
;    (book-deep-recur '() (lambda (x y) (append y (list x)))
;                    (lambda (x y) (append y (list x))))))
;(reverse-all '((1 2 3) (4 5) ((6 7 8) (9 10 11))))
;;   ==> '(((11 10 9) (8 7 6)) (5 4) (3 2 1))
;; (reverse-all '(x (y (z (((a) b) c)))))
;;   ==> '((((c (b (a))) z) y) x)

;; Exercise 7.31
(display "Exercise 7.31 not done\n")
;; ((flat-recur 1 +) '(1 2 3 4)) ==> 11
;; ((flat-recur '(a b c) cons) '(w x y z)) ==> '(w x y z a b c)
(define flat-recur 0)

;; Problem 1
(define tail-recur
  (lambda (bpred xproc aproc acc0)
    (lambda (x)
    (if (bpred x)
      acc0
      ((tail-recur bpred xproc aproc (aproc x acc0)) (xproc x))))))

(define reverse (tail-recur null? cdr (lambda (x acc) (cons (car x) acc)) '()))
(define iota (tail-recur zero? sub1 cons '()))

;(equal? (iota 100) (reverse (reverse (iota 100))))
; ==> #t
;(map iota '(1 2 3 4))
; ==> '((1) (1 2) (1 2 3) (1 2 3 4))

;; Problem 2
(define disjunction2
  (lambda (pred1 pred2)
    (lambda (x)
      (if (or (pred1 x) (pred2 x))
          #t
          #f))))

;(map (disjunction2 positive? (lambda (x) (= x -10))) '(-10 -5 0 5 10 15))
;   ==> '(#t #f #f #t #t #t)

;; Problem 3 
(display "Problem 3 not done\n")
;; variadic function
(define disjunction
  (lambda preds
    (lambda (x)
      (cond
        (((car preds) x))
        (else
          (cdr preds))))))

;((disjunction number? odd?) '(a (b c d)))
;(map (disjunction pair? number? null?) '(a (b c d) e () f () g (((h))) 1))
;;   ==> '(#f #t #f #t #f #t #f #t #t)

;; Problem 4
(define (matrix-map f a)
  (map (lambda (x) (map f x)) a))
  
;(matrix-map (lambda (x) (+ x 1)) '((1 2 3) (4 5 6) (7 8 9)))
;;   ==> '((2 3 4) (5 6 7) (8 9 10))
;(matrix-map (lambda (x) (* x 2)) '(() () () ()))
;;   ==> '(() () () ())

;; Problem 5
(define fold
      (lambda (seed proc)
        (letrec
          ((pattern
            (lambda (ls)
              (if (null? ls)
                  seed
                  (proc (car ls)
                        (pattern (cdr ls)))))))
pattern)))

(define member?
  (lambda (item ls)
    (if (null? ls)
        #f
        (or (equal? (car ls) item)
            (member? item (cdr ls))))))

(define delete-helper
  (lambda (item lst)
    (if (member? item lst)
        lst
        (cons item lst))))

(define delete-duplicates (fold '() delete-helper))

;(delete-duplicates '(x y x x y x))
; ==> '(x y) OR '(y x)

(display "Assoc not done\n")
(define assoc-helper
  (lambda (item lst)
    (cond
      ((equal? (caar lst) item) (car lst))
      ((null? lst) #f)
      (else
        (assoc-helper item (cdr lst))))))

;(assoc-helper 'b '((a 3) (b 3) (c 4) (b 5)))
;(define assoc
;  (lambda (x ls)
;    (let ((assoc-helper (lambda () _____))
 ;         (assoc-final (fold #f assoc-helper)))
 ;       (assoc-final ls))))


;(assoc 'a '((x 4) (y 5) (z 6) (a 7) (b 8) (a 9)))
;; ==> '(a 7)

;; Problem 6
;; Only use apply, select, map, filter, outer-product and iota
(define select
 (lambda (pred)
  (lambda (ls0 ls1)
   (map cdr
    (filter
     (lambda (x) (pred (car x)))
      (map cons ls0 ls1))))))

((select even?) (iota 8) '(a b c d e f g h))
; (b d f h)

(define make-row
  (lambda (proc x ys)
    (map (lambda (y) (proc x y)) ys)))

(define outer-product
  (lambda (proc)
    (lambda (xs ys)
    (map (lambda (x)
       (map (lambda (y) (proc x y)) ys)) xs))))

((outer-product *) '(1 2) '(3 4))

(define length 
  (lambda (ls)
    (apply + (map (lambda (x) 1) ls))))

(define sum-of-squares
  (lambda args
    (apply + (map (lambda (x) (* x x)) args))))

(define avg
  (lambda args
    (/ (apply + args) (apply + (map (lambda (x) 1) args)))))

(define avg-odd
  (lambda args
    (/ (apply + (filter odd? args)) (apply + (map (lambda (x) 1) (filter odd? args))))))

;; (define shortest 0)

(define avg-fact
  (lambda args
    (/ (apply + (map (lambda (ls) (apply * ls)) (map iota args))) (apply + (map (lambda (x) 1) args)))))

(define tally
 (lambda (pred)
  (lambda (ls)
   (apply +
    (map (lambda (x) (if (pred x) 1 0))
     ls)))))
     
;; (define list-ref 0)

;; (apply avg-fact (apply shortest (map iota '(5 20 7 3 23 7 8)))) ==> 3
;(apply sum-of-squares (list-ref (map iota (iota 20)) 13))
; ==> 819