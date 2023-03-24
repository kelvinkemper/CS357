#lang racket

(define number
  (lambda args
    (length args)))
(number)
(number 'a)
(number 'f 'o 'o)

(define sum-of-squares
  (lambda (a b c d)
    (+ (* a a) (* b b) (* c c) (* d d))))
(display "\nsum of squares\n")
(sum-of-squares 1 2 3 4)

(define sum-of-squares-c
    (lambda (x)
        (lambda (y)
            (lambda (z)
                (lambda (a)
                    (+ (* x x) (* y y) (* z z) (* a a)))))))
((((sum-of-squares-c 1) 2) 3) 4)

(define curry4
    (lambda (proc)
        (lambda (a)
            (lambda (b)
                (lambda (c)
                    (lambda (d)
                        (proc a b c d)))))))
(((((curry4 sum-of-squares) 1) 2) 3) 4)

(display "\nfold\n")
(define fold
  (lambda (proc seed)
    (letrec
      ((pattern
        (lambda (ls)
          (if (null? ls)
              seed
              (proc (car ls) (pattern (cdr ls)))))))
        pattern)))

(define snoc
  (lambda (ls x)
    (
      (fold cons (cons x '())) ;; inputted list 
      '(69 69)                       ;; x
      )
  )
)

(define snoc-add1
  (fold cons (cons 1 '())))
(snoc-add1 '(1 2 3))

(snoc '(1 2 3) '(3 4 5))

(define take-while-even (fold (lambda (fst prev-sol) (if (even? fst) (cons fst prev-sol) '())) '()))
(define take-while-odd (fold (lambda (fst prev-sol) (if (odd? fst) (cons fst prev-sol) '())) '()))
(take-while-even '(2 4 6 1 10 12))
(take-while-odd '(3 2 4 6 1 3 10 12))

(define take-while-c
  (lambda (pred)
    (fold (lambda (fst prev-sol) (if (pred fst) (cons fst prev-sol) '())) '())))

(define take-while-fr
  (lambda (pred ls)
    ((take-while-c pred) ls)))



(define take-while
  (lambda (pred ls)
    ((fold (lambda (fst prev-sol) (if (pred fst) (cons fst prev-sol) '())) '()) ls)))

(take-while even? '(2 4 6 7 8 9))
(take-while number? '(0 1 2 3 a b 3 4 5))
