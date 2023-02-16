#lang racket

;; TAIL POSITIONS

;; understand pair? vs list?
;; eq? equal? eqv? (=)


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
    (cons (cons 'lambda (cons names (cons body '()) vals))))))


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