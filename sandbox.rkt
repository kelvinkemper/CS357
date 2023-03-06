#lang racket

(define yesser '((1 2 3) (8)))

"Scope"
(define a 100)
a
(+ 7 a)

(let
    ((a 4)
     (b 3))
    (+ a b))


(define tester
    (lambda (ls)
        (list (if (pair? (car ls))
                (tester (car ls))
                (car ls)))))


(define dot
  (lambda v0
    (lambda v1
      (apply + (map * v0 v1)))))

;((dot '(8 9 10)) '(1 2 3))



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

(define my-length (fold 0 (lambda (x prev-len) (+ 1 prev-len))))
;; seed/base case 0
;; 

(define book-deep-recur
  (lambda (seed item-proc list-proc)
    (letrec
        ((helper
          (lambda (ls)
            (if (null? ls)
                '()
                (let ((a (car ls)))
                  (if (or (pair? a) (null? a))
                      (list-proc (helper a) (helper (cdr ls)))
                      (item-proc a (helper (cdr ls)))))))))
        helper
    )
  )
)

(define reverse-all-book
  (lambda (ls)
    (if (null? ls)
        '()
        (append 
          (reverse-all-book (cdr ls)) ; y
          (list (if (pair? (car ls)) 
                          (reverse-all-book (car ls))
                          (car ls))))
        )))

(define reverse-all
  (lambda (ls)
    (book-deep-recur '() ls (lambda (x y) (append y (list (if (pair? (car ls)) x (car ls))))))
                         ))

(reverse-all '((1 2 3) (4 5) ((6 7 8) (9 10 11))))