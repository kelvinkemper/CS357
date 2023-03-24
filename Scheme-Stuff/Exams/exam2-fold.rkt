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

;(define my-snoc
;  (lambda ))
;; (snoc ’(1 2 3) 0)

;; (snoc ’(a b c) ’(d))

(define snoc-0
  (lambda (ls)
    (if (null? ls) 
        (cons 0 '())
        (cons (car ls) (snoc-0 (cdr ls)))
        )))

(snoc-0 '(1 2 3 4))

(define snoc-0-f (fold cons (cons 0 '())))
(snoc-0 '(1 2 3 4))

(define snoc-maker
  (lambda (x)
    (fold cons (cons x '()))))

((snoc-maker 0) '(1 2 3 4))

(define snoc
  (lambda (ls x)
    ((snoc-maker x) ls)))
(snoc '(1 2 3) 34)

(define take-while-even (fold (lambda (x y) (if (even? x) (cons x y) '())) '()))


;; duplicate elements - repeats all elements of a list twice
;; '(1 2 3) => '(1 1 2 2 3 3)
(define duplicate-element
  (lambda (ls)
    (if (null? ls)
        '()
        (cons (car ls) (cons (car ls) (duplicate-element (cdr ls)))))))
        ;; (lambda (fst prev) (cons fst (cons (fst prev)))

;; product-elements
;; '(1 2 3 4) ==> 24

(define duplicate-element-fold (fold (lambda (x y) (cons x (cons x y))) '()))
(duplicate-element-fold '(1 2 3))

(define fold-tree
  (lambda (base-val leaf-proc fork-proc)
    (letrec ((helper
      (lambda (sexpr)
        (cond ((null? sexpr) base-val)
              ((pair? sexpr) (fork-proc (helper (car sexpr)) (helper (cdr sexpr))))
              (else (leaf-proc sexpr))
        )
        )))
      helper
    )
  )
)

(define flatten-tree (fold-tree '() list append))
(flatten-tree '((a) ((b)) (((c)))))

