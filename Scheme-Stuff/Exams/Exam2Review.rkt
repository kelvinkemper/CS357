#lang racket

(define fold
    (lambda (proc seed)
        (letrec
            ((pattern
                (lambda (ls)
                    (if (null? ls)
                        seed
                        (proc (car ls)
                                (pattern (cdr ls)))))))
            pattern)))
;; takes a function of 1 argument and returns the function of 2 arguments
(define uncurry (lambda (f) (lambda (x y) ((f x) (f y)))))

;;snoc opposite of cons
;; cannot just be a the normal process since we need 2 inputs
;; (snoc '(1 2 3) 4) ==> '( 1 2 3 4)
;; (snoc-c '(1 2 3)) 4) ==> '(1 2 3 4)
(define snoc-2
 (lambda (ls x)
    ((fold cons (list x)) ls)))


(define f (lambda (x) (append '(2 3 4 5 6) (cons x '()))))
(define g (compose (lambda (ls) (cons 1 ls)) f))
(define h (compose (lambda (ls) (cons 0 ls)) g))

;; value being returned from fold is a function == pattern
(define swap (lambda (f) (lambda (x y) (f y x))))
;; 
(define reverse-fold (fold (lambda (x ls) (snoc-2 ls x)) '()))
;; or another way
(define reverse-fold2 (fold (swap snoc-2) '()))

(define fold-tree
    (lambda (base-val leaf-proc fork-proc)
        (letrec ((helper
            (lambda (sexpr)
                (cond ((null? sexpr) base-val)
                      ((pair? sexpr) (fork-proc (helper (car sexpr)) (helper (cdr sexpr))))
                      (else (leaf-proc sexpr))
                      ))))
                helper)))

(define add (fold-tree 0 identity +))
(define number-tree (cons 1 (cons (cons 2 3) (cons 4 (cons 5 6)))))
(define copy (fold-tree '() identity cons))
(define size (fold-tree 0 (lambda (x) 1) +))
(define depth (fold-tree 0 (lambda (x) 0) (lambda (left-dep right-dep) (add1 (max left-dep right-dep)))))
(define count-c (lambda (pred) (fold-tree 0 (lambda (x) ((if (pred x) 1 0)) +))))
(define count (uncurry count-c))




(define number
    (lambda args
        (length args)))
;(number)


;; (flatten-all '(1 ((((2 3)))) (4) 5)) => '(1 2 3 4 5)
(define flatten-all
  (lambda (ls)
    (cond ((null? ls) '())
          ((pair? (car ls))
           (append (flatten-all (car ls))
                   (flatten-all (cdr ls))))
          (else
           (cons (car ls)
                 (flatten-all (cdr ls)))))))

(flatten-all '(1 ((((2 3)))) (4) 5))
;; => '(1 2 3 4 5)


