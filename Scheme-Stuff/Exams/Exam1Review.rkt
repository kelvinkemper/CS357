#lang racket

;; every value is consider #t such as 'cat 'dog 5
;; only #f (nothing else) is false in scheme 

;; TAIL POSITIONS

;; understand pair? vs list?
;; eq? equal? eqv? (=)
;; (=) for NUMBERS only!
;; eq? -> strict, compares pointers in memory location **pointers
;; equal? can be used for lists **lists


;; defining bindings as it evaluates the expression, then bindings are gone
;; (let ((x 1) (y 2) (z 3)) (+ x y z))

;; let* available sequentially, more open than "let"
;;(let* ((x 1) (y 2) (z 3)) (+ x y z))

;; letrec allows recursive definitions for functions
;;(letrec ((x 1) (y z) (z 3)) (+ x y z))

;; let-> lambda
(define let->lambda
  (lambda (expr)
    (let
      ((names (map car (cadr expr)))
      (vals  (map cadr (cadr expr)))
      (body  (caddr expr)))
    (cons (cons 'lambda (cons names (cons body '()))) vals))))




(define deep-flatten
  (lambda (ls)
    (cond
      ((null? ls) '())
      ((pair? (car ls)) (append (deep-flatten (car ls)) (deep-flatten (cdr ls))))
      (else (cons (car ls (deep-flatten (cdr ls))))))))

;; (( 1 2 3) 4 5)
;; (append (deep-flatten '(1 2 3)) (deep-flatten '(4 5)))
;; (deep-flatten '(1 2 3)) => 

;;(assoc 'cat '((dog bark) (duck quack) (cat meow) (cow moo)))

(define assoc
  (lambda (item ls)
    (cond
      ((null? ls) #f)
      ((eq? (caar ls) item) (car ls))
      (else (assoc item (cdr ls))))))

;(define sublist?
 ; (lambda (ls1 ls2)
  ;  (cond
   ;   ((null? ls1) #t)
    ;  ((null? ls2) #f)
     ; ((and (eq? (car ls1) (car ls2)) ))))

;; ls1 => '(a b)       '(a b c d e)
;; ls1 => '(a b)       '(c d a b e)
(define prefix?
  (lambda (ls1 ls2)
    (cond
      ((null? ls1) #t)
      ((null? ls2) #f)
      (else (and (eq? (car ls1 (car ls2)) (prefix? (cdr ls1) (cdr ls2))))))))

(define sublist?
  (lambda (ls1 ls2)
    (cond
      ((null? ls1) #t)
      ((null? ls2) #f)
      ((prefix? ls1 ls2) #t)
      (else (sublist? ls1) (cdr ls2)))))


;; '(a b)    '(c b a)
;; (helper 5 0)
;; is 5 > 0? returns helper(4 5)
;; is 4 > 0? returns helper(3 9)
;; is 3 > 0? returns helper(2 12)
;; is 2 > 0? returns helper(1 14)
;; is 1 > 0? returns helper(0,15)
;; is 0 > 0? returns acc = 15
(define barney-tail
  (lambda (x)
    (letrec ((helper (lambda (y acc) (if (> y 0) (helper (- y 1) (+ y acc)) acc))))
      (helper x 0))));; starting with the accumulator at zero



(define foo (cons 1 2))
(define bar (cons 1 2))
(define bert 'foo)
(define ernie bar)




; list - can be an empty list or a pair whose second element is a list, must have '() at end
; pair - combination of two values
; append - must combine a list with atom/list
;; equal? - 

(list? (cons 1 '()))
;; #t - lists checks if it's a list that ends with '()

(let* ((x (cons 1 2)) (y x)) 
      (eq? x y))
;; #t - x = (cons 1 2), y = x = (cons 1 2) y points to x that poitns to (cons 1 2)

;(append 1 (list 2 3))
;; #f 1st argument must be a list, 2nd can be either list or atom

(and 'cat #t #f) 
;; #f contains a false

((lambda (x y) y) 2 3)
;; 3

(if 1 2 3)
;; if (first thing = true? true is anything excep #f) then 2 otherwise 3

;(1 2)
;; no procedure or special case

(eq? (cons 'kirk 'spock) (cons 'kirk 'spock))
;; #f - different pointer locations

(equal? 1 1)
;; #t

;(append (1 2) (3 4))
;; error since two arguments are not list must have (quote) or '() around arguments

(cons 1 2)
;; '(1 . 2)
;(cadr 1 2)
;; incorrect argument mismatch

(or #t #f 'dog)
;; #t

(cond (#f 0) (#t 1) (else 2))
;; 1 since always true

(cadr (list 1 2 3))
;; 2

(car '(((1))))
; '((1))

(cdr '(((1))))
; '()

(list? (cons 1 2))
;#f

(list 1 2 3)
;'(1 2 3)

(append)
; '() - identity


(+)
; 0

(*)
; 1