#lang racket

(define iota
  (lambda (n)
    (letrec
      ((loop 
        (lambda (n acc)
          (if (= n 0)
              acc
              (loop (sub1 n) 
                    (cons n acc))))))
      (loop n '()))))
(iota 8)

(define select
    (lambda (pred)
        (lambda (ls0 ls1)
            (map cdr
                (filter
                    (lambda (x) (pred (car x)))
                    (map cons ls0 ls1))))))

 ((select even?) (iota 8) '(a b c d e f g h))

(define powerset
    (lambda (xs)
        (if (null? xs)
            '(())
            (let ((half (powerset (cdr xs))))
                (append (map (lambda (x) (cons (car xs) x))
                             half)
                        half)))))

(define make-change
    (lambda (amount coins)
        (let ((lss (powerset coins)))
            (car (select
                    (lambda (x) (= x amount)))
                    (map (lambda (ls) (apply + ls)) lss)
                    lss))))

