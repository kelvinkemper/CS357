#lang racket

(define count
   (let ((next 0))
     (lambda ()
       (let ((v next))
         (set! next (+ next 1))
         v))))

(count)
(count)
(count)

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

(define snoc-0 (fold cons '(0)))
(define snoc-a (fold cons '(a)))
(define snoc-b (fold cons (cons 'b '())))

(display "\nfold\n")
(snoc-0 '(1 2 3))
(snoc-a '(1 2 3))
(snoc-b '(1 2 3))

(define snoc
  (lambda (ls x)
    (
      (fold cons (cons x '())) 
      ls
      )
  )
)

(define add1toX
  (lambda (x)
    (+ 1 x)))
(add1toX 5)

(snoc '(1 2 3) '(3 4 5))